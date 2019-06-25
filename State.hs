{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module State (Player, newPlayer,
                playerno, hand, deck, discarded, played, actions, purchases, money, pendingChoices,
              GameState (GS),
                players, table,
              State (..), scrambleState, newGame) where

import GHC.Generics
import Data.Aeson
import Control.Lens
import Data.List

import Cards
import PointedList
import Data.List.PointedList (PointedList)

data Player = Player { _playerno :: Int,
                       _hand :: [Card],
                       _deck :: [Card],
                       _discarded :: [Card],
                       _played :: [Card],
                       _actions :: Int,
                       _purchases :: Int,
                       _money :: Int,
                       _pendingChoices :: [ChoiceFlag]}
    deriving (Generic, Show)
instance ToJSON Player
instance FromJSON Player
instance Eq Player where
    p1 == p2 = _playerno p1 == _playerno p2

makeLenses ''Player

newPlayer :: Int -> Player
newPlayer i = Player i [] [] ((take 7 $ repeat Copper) ++ (take 3 $ repeat Estate)) [] 1 1 0 []

data GameState = GS {_players :: PointedList Player,
                     _table :: [(Card, Int)]} --Card, Amount
    deriving (Generic, Show)
makeLenses ''GameState

instance ToJSON GameState
instance FromJSON GameState

data State = JoiningState [Player]
           | GameState GameState
           | EndState GameState [(Int, Int)] --scoreboard; playerno, score;
    deriving (Generic, Show)
makeLenses ''State

instance ToJSON State
instance FromJSON State



scrambleState :: Int -> State -> State
scrambleState plr s = case s of
    JoiningState _ -> s
    GameState gs -> GameState $ gs & players %~ fmap plrscramble
        where plrscramble p = if p ^. playerno == plr then
                                p & deck %~ sort
                              else
                                let newdeck = p ^. hand ++ p ^. deck & sort in p & deck .~ newdeck & (hand . each) %~ (const Copper)
    EndState _ _ -> s

newGame :: State
newGame = JoiningState []
