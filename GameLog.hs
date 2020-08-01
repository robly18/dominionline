{-# LANGUAGE DeriveGeneric #-}

module GameLog (Action(..), Event(..), DataSet(..), DeckChange(..), GameLog) where

import GHC.Generics
import Data.Aeson

import Cards
import Actions

import Data.Vector


data Event = DrawEvent Int Card --the card that has been drawn
           | JoinEvent Int
           | PlayedCardEvent Int Card
           | PlayedCardEffect Int Effect
           | PlayerChangeEvent DataSet
           | DeckChangeEvent Int DeckChange
           | PushChoiceEvent Int ChoiceFlag
           | EndTurnEvent
    deriving (Generic, Show)
instance ToJSON Event

data DataSet = MoneySet Int
             | ActionsSet Int
             | PurchasesSet Int
    deriving (Generic, Show)
instance ToJSON DataSet

data DeckChange = DCDraw Card
                | DCDiscard Int
                | DCShuffle
    deriving (Generic, Show)
instance ToJSON DeckChange

type GameLog = Vector Event
