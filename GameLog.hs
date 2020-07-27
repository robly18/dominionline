{-# LANGUAGE DeriveGeneric #-}

module GameLog (Action(..), Event(..), DataDelta(..), GameLog) where

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
           | DrawEvent Int Card --the card that has been drawn
           | JoinEvent Int
           | PlayedCardEvent Int Card
           | PlayedCardEffect Int Effect
           | PlayerChangeEvent Int DataDelta
    deriving (Generic, Show)
instance ToJSON Event

data DataDelta = MoneyDelta Int
               | ActionDelta Int
               | PurchasesDelta Int
    deriving (Generic, Show)
instance ToJSON DataDelta

type GameLog = Vector Event
