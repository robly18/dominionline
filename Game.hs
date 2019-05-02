{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Rank2Types #-}
module Game (newGame, act, State, encode, decode, joinGame,
            Action, RL,
            module Control.Monad.Writer) where


import qualified Data.Sequence as S
import Data.Maybe
import Data.Aeson (encode, decode, ToJSON, FromJSON)
import Data.List
import Data.Foldable

import Control.Monad
import Control.Monad.Random.Lazy (lift, RandT, StdGen)
import Control.Monad.Writer
import System.Random.Shuffle

import Control.Lens

import GHC.Generics

type RL = RandT StdGen (Writer [String])

data Card = Copper | Victory | Village | Forge --todo
    deriving (Generic, Show)
instance ToJSON Card
instance FromJSON Card

data Player = Player { _playerno :: Int,
                       _hand :: [Card],
                       _deck :: [Card],
                       _discarded :: [Card],
                       _played :: [Card],
                       _actions :: Int,
                       _money :: Int }
    deriving (Generic, Show)
instance ToJSON Player
instance FromJSON Player
instance Eq Player where
    p1 == p2 = _playerno p1 == _playerno p2

makeLenses ''Player

draw :: Player -> RL Player
draw p = case _deck p of
            (c:cs) -> (lift $ tell ["Drawing one"]) >> return p {_hand = c : _hand p, _deck = cs}
            [] -> case _discarded p of
                    [] -> (lift $ tell $ ["Ran out of cards"]) >> return p
                    _ -> (lift $ tell $ ["Shuffling discarded"]) >> (shuffleDiscarded p) >>= draw

shuffleDiscarded :: Player -> RL Player
shuffleDiscarded p = do newdeck <- shuffleM (_discarded p ++ _deck p)
                        return $ p {_discarded = [], _deck = newdeck}

discardDraw :: Player -> RL Player
discardDraw p = do lift $ tell $ ["discard drawing"]
                   let pp = p {_hand = [], _played = [], _discarded = _hand p ++ _played p ++ _discarded p}
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

playerN :: Int -> Lens' State Player
playerN n = lens getter setter
    where   getter s = players s !! n
            setter s p = s {players = set (element $ n) p (players s)}

currPlayer :: Lens' State Player
currPlayer = lens getter setter
    where   getter s = view (playerN $ playing s) s
            setter s p = set (playerN $ playing s) p s


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

act (JoiningState plrs) (plr, StartGame) = return $ GameState plr plrs [Copper, Copper, Copper, Victory, Victory, Victory]

act s (plr, Say x) = do lift $ tell [show plr ++ ": " ++ x]
                        return s

act s@(GameState plr _ _) (plr2, action) = if plr /= plr2 then return s else
        case action of
            EndTurn -> nextPlayer s
            Buy -> buyCard s
            Play i -> playCard s i
            _ -> return s

act s _ = return s

nextPlayer (GameState p plrs stack) = do lift $ tell ["Player " ++ show p ++ " ends their turn."]
                                         let p2 = (p+1) `mod` (length plrs)
                                         lift $ tell ["It's player " ++ show p2 ++"'s turn."]
                                         newplrp <- discardDraw $ plrs !! p
                                         let newnewplrp = newplrp {_actions = 0, _money = 0} --i hate my life
                                         let newplrs = set (element p) newplrp plrs
                                         return $ GameState p2 newplrs stack --todo: send played to discarded!!

getPlayer :: Int -> [Player] -> Player
getPlayer i plrs = plrs !! i

getCurrentPlayer :: State -> Player
getCurrentPlayer s = players s !! playing s

playCard :: State -> Int -> RL State
playCard s@(GameState p plrs stack) i = --todo sanity checks before using !!
    let player = getPlayer p plrs
        itshand = _hand player
        (newhand, card) = ((take i itshand) ++ (drop (i+1) itshand), itshand !! i)
        newplayer = player {_hand = newhand, _played = card : _played player}
        newplrs = (take p plrs) ++ [newplayer] ++ (drop (p+1) plrs) in
    --act on the card i guess
    lift (tell ["Player " ++ show p ++ " played " ++ show card])
    >> (actOnCard p card (s {players = newplrs}))


actOnCard :: Int -> Card -> State -> RL State
actOnCard plr Copper = return . over (currPlayer . money) (+1) --todo draw. todo figure out how to make monads work with lenses @_@
actOnCard plr Village = return . (over currPlayer $ (over actions (+2)) . (over actions (+0))) --todo draw. todo figure out how to make monads work with lenses @_@
actOnCard _ _ = return

buyCard :: State -> RL State
buyCard s = do  lift $ tell ["Player " ++ show (playing s) ++ " buys a card."]
                let plr = getCurrentPlayer s
                let pn = playing s
                let plrs = players s
                if _money plr >= 1 then do
                    let newplr = plr { _money = _money plr - 1, _played = _played plr ++ [head $ table s] }
                    let newplrs = (take pn plrs) ++ [newplr] ++ (drop (pn+1) plrs)
                    return s {players = newplrs, table = tail $ table s }
                else do
                    return s

joinGame :: State -> Writer [String] (Maybe Int, State)
joinGame (JoiningState plrs) = do let plrno = length plrs
                                  tell ["Player " ++ show plrno ++ " joins."]
                                  return (Just plrno, JoiningState $ plrs ++ [newPlayer $ plrno])
joinGame s = return (Nothing, s)

newPlayer :: Int -> Player
newPlayer i = Player i [Copper, Victory, Copper, Copper] [] [] [] 0 0

