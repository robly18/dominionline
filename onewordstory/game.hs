{-# LANGUAGE DeriveGeneric #-}
module Game (newGame, act, State, encode, decode, joinGame,
            Action (Write, Start, Poll)) where

import Data.Array
import Data.Maybe
import Data.Aeson

import GHC.Generics

data Action = Write String | Start | Poll
    deriving (Generic, Show)

instance ToJSON Action
instance FromJSON Action

data State = State {playerno :: Int, playing :: Int, story :: [String]}
           | StartState {playerno :: Int}
    deriving (Generic, Show)

instance ToJSON State
instance FromJSON State

newGame :: State
newGame = StartState 0

joinGame :: State -> (Maybe Int, State)
joinGame s@(State _ _ _) = (Nothing, s)
joinGame (StartState plrno) = (Just plrno, StartState (plrno+1))

act :: State -> (Int, Action) -> State
act st (_, Poll) = st
act st@(State plrno playing story) (plr, Write s)
 | playing /= plr = st
 | otherwise      = State (plrno) ((playing + 1) `mod` plrno) (story++[s])
act s@(State _ _ _) (_, _) = s
act s@(StartState plrno) (plr, action) = case action of
    Start -> State plrno plr []
    _     -> s