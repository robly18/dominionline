{-# LANGUAGE DeriveGeneric #-}

module GameLog (Action(..), Event(..), GameLog) where

import GHC.Generics
import Data.Aeson

import Cards

import Data.Vector

data Action = Poll
            | PollCard Card
            | StartGame
            | Say String
            | Play Int --play the nth card in one's hand
            | EndTurn
            | Buy Int --buy from the nth pile in the deck
            | Choose Choice
            | NextGame
    deriving (Generic, Show)
instance ToJSON Action
instance FromJSON Action

data Event = PlayerAction Int Action
           | DrawEvent Int
           | JoinEvent Int
           | PlayedCardEvent Card
    deriving (Generic, Show)
instance ToJSON Event

type GameLog = Vector Event
