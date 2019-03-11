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

data State = State Int
    deriving (Generic, Show)

newGame :: State
newGame = State 0

instance ToJSON State
instance FromJSON State

data Action = Poll | Say String
    deriving (Generic, Show)
instance ToJSON Action
instance FromJSON Action

act :: State -> (Int, Action) -> RL State
act s (plr, a) = case a of
                    Poll -> return s
                    Say x -> do lift $ log $ show plr ++ ": " ++ x
                                return s

joinGame :: State -> Log (Maybe Int, State)
joinGame (State s) = do log $ "Player " ++ show s ++ " joins."
                        return (Just s, State $ s+1)

