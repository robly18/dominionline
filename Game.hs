{-# LANGUAGE DeriveGeneric #-}
--module Game (newGame, act, State, encode, decode, joinGame,
--            Action (Write, Start, Poll)) where

import Prelude hiding (log)

import Data.Sequence
import Data.Maybe
import Data.Aeson (ToJSON, FromJSON)

import Control.Monad

import GHC.Generics

type Deck = [Int]

deck :: Int -> Deck --argument is number of cards of each suit. there are 4 suits.
deck n = [i | i <- [1..n], j <- [1..4]]

data Action = Request Int | Start | Poll
    deriving (Generic, Show)

instance ToJSON Action
instance FromJSON Action

type PlayerState = (Deck, [Int]) --Hand, Points made

data Table = Table {players :: Seq PlayerState, playing :: Int, pool :: Deck}
    deriving (Generic, Show)
instance ToJSON Table
instance FromJSON Table

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

data State = State Table
           | StartState Int
    deriving (Generic, Show)

instance ToJSON State
instance FromJSON State

newGame :: State
newGame = StartState 0

joinGame :: State -> Log (Maybe Int, State)
joinGame (StartState plrno) = do log $ "Player "++show plrno++" joins the game!"
                                 return (Just plrno, StartState (plrno+1))
joinGame s = return (Nothing, s)


act :: State -> (Int, Action) -> Log State
act st (_, Poll) = return st
act (State t) (plr, Request card) = (fmap State) $ request plr card t
act s@(StartState plrno) (plr, action) = case action of
    Start -> (log $ "Player "++show plr++" has started the game." ) >> (return $ State $ newTable plrno)
    _     -> return s
act s (_, _) = return s

request :: Int -> Int -> Table -> Log Table
request _ _ _ = undefined

newTable :: Int -> Table
newTable _ = undefined