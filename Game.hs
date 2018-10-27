{-# LANGUAGE DeriveGeneric #-}
module Game (newGame, act, State, encode, decode, joinGame,
            Action (Write, Start, Poll)) where

import Data.Array
import Data.Maybe
import Data.Aeson

import GHC.Generics

type Deck = [Int]

deck :: Int -> Deck --argument is number of cards of each suit. there are 4 suits.
deck n = [i | i <- [1..n], j <- [1..4]]

data Action = Request Int | Start | Poll
    deriving (Generic, Show)

instance ToJSON Action
instance FromJSON Action

data State = State {playerno :: Int, playing :: Int, decks :: [Deck], points :: [Int], table :: Deck, log :: [String]}
           | StartState {playerno :: Int}
    deriving (Generic, Show)

data ShowState = ShowState {playerno :: Int, playing :: Int, decks :: [Int], points :: [Int] Table :: Int, mydeck :: Deck, log :: [String]}
               | StartSState {playerno :: Int}
    deriving (Generic, Show)

instance ToJSON ShowState
instance FromJSON ShowState

newGame :: State
newGame = StartState 0

joinGame :: State -> (Maybe Int, State)
joinGame s@(State _ _ _) = (Nothing, s)
joinGame (StartState plrno) = (Just plrno, StartState (plrno+1))

act :: State -> (Int, Action) -> State
act st (_, Poll) = st
act st (plr, Request card) = request plr card st
act s@(State _ _ _) (_, _) = s
act s@(StartState plrno) (plr, action) = case action of
    Start -> State plrno plr []
    _     -> s

request :: Int -> Int -> State -> State
request plr card s@(State plrno playing decks points table log) = let whohas = findIndices (elem card) decks in
    if playing `elem` whohas then
        let others = delete playing whohas in
        if others == [] then goFish plr card s
        else give card plr s
    else
        s
request _ _ s = s

goFish :: Int -> Int -> State -> State
goFish plr card s@(State plrno playing decks points table log) = case table of
    []      -> nextPlr $ State plrno playing decks points table (log++["Player "++show plr++" asked for "++show card++", but there were none."])
    (nc:nt) -> if nc == card then
                State plrno playing (addTo nc playing decks) --argh. todo


nextPlr :: State -> State
nextPlr (State plrno playing decks points table log) =  State plrno ((playing+1)`mod`plrno) decks points table log
