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

import GHC.Generics

import Control.Lens

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

data Player = Player { _playerno :: Int,
                       _hand :: [Card],
                       _deck :: [Card],
                       _discarded :: [Card],
                       _played :: [Card],
                       _money :: Int }
    deriving (Generic, Show)
instance ToJSON Player
instance FromJSON Player

data State = JoiningState [Player]
           | GameState Int [Player] [Card] -- playerturn, player list, cards on the table
    deriving (Generic, Show)

newGame :: State
newGame = JoiningState []

instance ToJSON State
instance FromJSON State

data Action = Poll
            | Say String
            | Play Int --play the nth card in one's hand
            | EndTurn
    deriving (Generic, Show)
instance ToJSON Action
instance FromJSON Action

act :: State -> (Int, Action) -> RL State
act s (_  , Poll) = return s
act s (plr, Say x) = do lift $ log $ show plr ++ ": " ++ x
                        return s
act s (plr, EndTurn) = case s of
                        JoiningState _ -> return s
                        GameState _ _ _ -> nextPlayer s
act s (plr, Play i) = case s of
                        JoiningState _ -> return s
                        GameState _ _ _ -> playCard s plr i

nextPlayer (GameState p plrs stack) = do lift $ log $ "Player " ++ show p ++ " ends their turn."
                                         let p2 = (p+1) `mod` (length plrs)
                                         lift $ log $ "It's player " ++ show p2 ++"'s turn."
                                         return $ GameState p2 plrs stack --todo: send played to discarded!!

playCard :: State -> Int -> Int -> RL State
playCard s@(GameState p plrs stack) plr i = return s --todo: use State Monad
{-
	player_in_question = plrs[plr]
	card_played = player_in_question.hand.pop(i)
	player_in_question.played.push(card_played)
	//something  to do w card_played
-}
{- To do:
    extract card 'i' from plrs[i].hand
    move it to plrs[i].played
    act accordingly-}

joinGame :: State -> Log (Maybe Int, State)
joinGame (JoiningState plrs) = do let plrno = length plrs
                                  log $ "Player " ++ show plrno ++ " joins."
                                  return (Just plrno, JoiningState $ plrs ++ [newPlayer $ plrno])

newPlayer :: Int -> Player
newPlayer i = Player i [] [] [] [] 0

