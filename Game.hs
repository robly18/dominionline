{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Game (newGame, act, State, encode, decode, joinGame,
            Action, RL, Log, forget) where

import Prelude hiding (log)

import qualified Data.Sequence as S
import Data.Maybe
import Data.Aeson (encode, decode, ToJSON, FromJSON)
import Data.List
import Data.Foldable

import Control.Monad
import Control.Monad.Random.Lazy (lift, RandT, StdGen)
import System.Random.Shuffle

import Control.Lens

import GHC.Generics

data Log a = Log [String] a
    deriving (Generic, Show)
instance (ToJSON a) => ToJSON (Log a)
instance (FromJSON a) => FromJSON (Log a)
instance Functor Log where
    fmap f (Log l x) = Log l $ f x
instance Applicative Log where
    pure x = Log [] x
    (Log l1 f) <*> (Log l2 x) = Log (l1 ++ l2) (f x)
instance Monad Log where
    (Log l x) >>= f = let Log l2 y = f x in Log (l ++ l2) y
log :: String -> Log ()
log s = Log [s] ()
forget :: Log a -> a
forget (Log _ x) = x

type RL = RandT StdGen Log

data Card = Copper | Victory
    deriving (Generic, Show)
instance ToJSON Card
instance FromJSON Card

data Player = Player { playerno :: Int,
                       hand :: [Card],
                       deck :: [Card],
                       discarded :: [Card],
                       played :: [Card],
                       money :: Int }
    deriving (Generic, Show)
instance ToJSON Player
instance FromJSON Player
instance Eq Player where
    p1 == p2 = playerno p1 == playerno p2

draw :: Player -> RL Player
draw p = case deck p of
            (c:cs) -> (lift $ log $ "Drawing one") >> return p {hand = c : hand p, deck = cs}
            [] -> case discarded p of
                    [] -> (lift $ log $ "Ran out of cards") >> return p
                    _ -> (lift $ log $ "Shuffling discarded") >> (shuffleDiscarded p) >>= draw

shuffleDiscarded :: Player -> RL Player
shuffleDiscarded p = do newdeck <- shuffleM (discarded p ++ deck p)
                        return $ p {discarded = [], deck = newdeck}

discardDraw :: Player -> RL Player
discardDraw p = do lift $ log $ "discard drawing"
                   let pp = p {hand = [], played = [], discarded = hand p ++ played p ++ discarded p}
                   iterate (\pAnt -> draw =<< pAnt) (return pp) !! 5

data State = JoiningState [Player]
           | GameState {playing :: Int, --todo define cyclical list
                        players :: [Player],
                        table :: [Card]}
    deriving (Generic, Show)

newGame :: State
newGame = JoiningState []

instance ToJSON State
instance FromJSON State

data Action = Poll
            | StartGame
            | Say String
            | Play Int --play the nth card in one's hand
            | EndTurn
            | Buy
    deriving (Generic, Show)
instance ToJSON Action
instance FromJSON Action

act :: State -> (Int, Action) -> RL State --use maybe
act s (_  , Poll) = return s
act s (plr, StartGame) = case s of
                        JoiningState plrs -> return $ GameState plr plrs [Copper, Copper, Copper, Victory, Victory, Victory]
                        GameState _ _ _ -> return s
act s (plr, Say x) = do lift $ log $ show plr ++ ": " ++ x
                        return s
act s (plr, EndTurn) = case s of
                        JoiningState _ -> return s
                        GameState plr2 _ _ -> if plr == plr2 then nextPlayer s else return s
act s (plr, Play i) = case s of
                        JoiningState _ -> return s
                        GameState plr2 _ _ -> if plr == plr2 then playCard s i else return s
act s (plr, Buy) = case s of
                        JoiningState _ -> return s
                        GameState plr2 _ _ -> if plr == plr2 then buyCard s else return s

nextPlayer (GameState p plrs stack) = do lift $ log $ "Player " ++ show p ++ " ends their turn."
                                         let p2 = (p+1) `mod` (length plrs)
                                         lift $ log $ "It's player " ++ show p2 ++"'s turn."
                                         newplrp <- discardDraw $ plrs !! p
                                         let newplrs = set (element p) newplrp plrs
                                         return $ GameState p2 newplrs stack --todo: send played to discarded!!

getPlayer :: Int -> [Player] -> Player
getPlayer i plrs = plrs !! i

getCurrentPlayer :: State -> Player
getCurrentPlayer s = players s !! playing s

playCard :: State -> Int -> RL State
playCard s@(GameState p plrs stack) i = --todo sanity checks before using !!
    let player = getPlayer p plrs
        itshand = hand player
        (newhand, card) = ((take i itshand) ++ (drop (i+1) itshand), itshand !! i)
        newplayer = player {hand = newhand, played = card : played player}
        newplrs = (take p plrs) ++ [newplayer] ++ (drop (p+1) plrs) in
    --act on the card i guess
    lift (log $ "Player " ++ show p ++ " played " ++ show card)
    >> (actOnCard (s {players = newplrs}) p card)


actOnCard :: State -> Int -> Card -> RL State
actOnCard s plr Copper = let plrs = players s in
                         let newplayer = (\p -> p {money = money p + 1}) $ players s !! plr in
                         let newplrs = (take plr plrs) ++ [newplayer] ++ (drop (plr+1) plrs) in
                         return $ s {players = newplrs}
actOnCard s _ _ = return s

buyCard :: State -> RL State
buyCard s = do  lift $ log $ "Player " ++ show (playing s) ++ " buys a card."
                let plr = getCurrentPlayer s
                let pn = playing s
                let plrs = players s
                if money plr >= 1 then do
                    let newplr = plr { money = money plr - 1, played = played plr ++ [head $ table s] }
                    let newplrs = (take pn plrs) ++ [newplr] ++ (drop (pn+1) plrs)
                    return s {players = newplrs, table = tail $ table s }
                else do
                    return s

joinGame :: State -> Log (Maybe Int, State)
joinGame (JoiningState plrs) = do let plrno = length plrs
                                  log $ "Player " ++ show plrno ++ " joins."
                                  return (Just plrno, JoiningState $ plrs ++ [newPlayer $ plrno])
joinGame s = return (Nothing, s)

newPlayer :: Int -> Player
newPlayer i = Player i [Copper, Victory, Copper, Copper] [] [] [] 0

