{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Game (newGame, act, State, encode, decode, joinGame,
            Action (Request, Start, Poll), RL, Log, forget) where

import Prelude hiding (log)

import qualified Data.Sequence as S
import Data.Maybe
import Data.Aeson (encode, decode, ToJSON, FromJSON)
import Data.List
import Data.Foldable

import Control.Monad
import Control.Monad.Random.Lazy (lift, RandT, StdGen)
import System.Random.Shuffle

import GHC.Generics

import Control.Lens

type Deck = [Int]

deck :: Int -> Deck --argument is number of cards of each suit. there are 4 suits.
deck n = [i | i <- [1..n], j <- [1..4]]

data Action = Request Int | Start | Poll | Pass
    deriving (Generic, Show)

instance ToJSON Action
instance FromJSON Action

type PlayerState = (Deck, [Int]) --Hand, Points made

data Table = Table {_players :: S.Seq PlayerState, _playing :: Int, _pool :: Deck}
    deriving (Generic, Show)
instance ToJSON Table
instance FromJSON Table
makeLenses ''Table

data Log a = Log [String] a
    deriving (Generic, Show)
instance (ToJSON a) => ToJSON (Log a)
instance (FromJSON a) => FromJSON (Log a)
instance Functor Log where
    fmap f (Log l x) = Log l (f x)
instance Applicative Log where
    pure x = Log [] x
    (Log l1 f) <*> (Log l2 x) = Log (l1 ++ l2) (f x)
instance Monad Log where
    (Log l1 x) >>= f = let Log l2 y = f x in Log (l1 ++ l2) y
log :: String -> Log ()
log s = Log [s] ()
forget :: Log a -> a
forget (Log _ x) = x

type RL = RandT StdGen Log

data State = State Table
           | StartState Int
    deriving (Generic, Show)

instance ToJSON State
instance FromJSON State

newGame :: State
newGame = StartState 0

joinGame :: State -> Log (Maybe Int, State)
joinGame (StartState plrno) = (log $ "Player "++show plrno++" joins the game!") >>
                                return (Just plrno, StartState (plrno+1))
joinGame s = return (Nothing, s)


act :: State -> (Int, Action) -> RL State
act st (_, Poll) = return st
act (State t) (plr, Request card) = lift $ (fmap State) $ request plr card t --todo check if it's your turn!!
act (State t) (plr, Pass) = lift $ (fmap State) $ nextPlr t
act s@(StartState plrno) (plr, action) = case action of
    Start -> do lift $ log $ "Player "++show plr++" has started the game."
                nt <- newTable plrno
                return $ State nt
    _     -> return s
act s (_, _) = return s

request :: Int -> Int -> Table -> Log Table
request plr card t@(Table tplayers playing _) =
    if plr == playing then
     do let playersWhoHave = S.findIndicesL (elem card . fst) tplayers
        if plr `elem` playersWhoHave then
             do log $ "Player "++show plr++" requested "++show card++"."
                let others = delete plr playersWhoHave
                if others == [] then (log "But nobody has it...") >> goFish plr card t
                else do log $ show playersWhoHave --placeholder
                        let collected = [ c | (p,_) <- toList tplayers, c <- p, c == card]
                        let withoutCollected = over players (fmap $ over _1 $ filter (/= card)) t
                        log $ "They collected "++show (length collected)++" cards!"
                        return $ over players (S.adjust (over _1 $ (collected++)) plr) withoutCollected
        else return t
    else return t
        

nextPlr :: Table -> Log Table
nextPlr t = do let newPlr = ((view playing t) + 1) `mod` (length $ view players t)
               log $ "It's player "++show newPlr++"'s turn."
               return $ set playing newPlr t

goFish :: Int -> Int -> Table -> Log Table
goFish plr req t =   do let tpool = view pool t
                        case tpool of
                          [] -> nextPlr t
                          (x:xs) ->
                            do log "Go fish!"
                               let fished = set pool xs $
                                            over players (S.adjust (over _1 $ (x:)) plr) $
                                            t
                               if x == req then
                                 do log $"They got a "++show req++"! It's their turn again"
                                    return fished
                               else
                                 do log $ "Tough luck. It wasn't the card they wanted."
                                    nextPlr fished
                                    

newTable :: Int -> RL Table
newTable plrno = do lift $ log $ "The game has started!"
                    decklist <- shuffleM $ deck 10
                    let (players, tpool) = extractPlayers plrno decklist
                    return $ Table (S.fromList players) 0 tpool
        where extractPlayers 0 decklist = ([], decklist)
              extractPlayers n decklist = let (pl, nd) = extractPlayers (n-1) decklist in
                                            ((take 4 nd,[]):pl, (drop 4 nd))
