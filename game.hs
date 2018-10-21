{-# LANGUAGE DeriveGeneric #-}
module Game (newGame, act, State, encode,
            Action (Write, Start)) where

import Data.Array
import Data.Maybe
import Data.Aeson

import GHC.Generics

data Action = Write String | Join | Start

data State = State {playerno :: Int, playing :: Int, story :: [String]}
           | StartState {playerno :: Int}
    deriving (Generic, Show)

instance ToJSON State
instance FromJSON State

newGame :: State
newGame = State 1 0 []--StartState 0

act :: State -> (Int, Action) -> State
act st@(State plrno playing story) (plr, Write s)
 | playing /= plr = st
 | otherwise      = State (plrno) ((playing + 1) `mod` plrno) (story++[s])
act s@(State _ _ _) (_, _) = s
act s@(StartState plrno) (plr, action) = case action of
    Start -> State plrno plr []
    Join  -> StartState (plrno+1)
    _     -> s