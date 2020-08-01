{-# LANGUAGE DeriveGeneric #-}

module Actions (Action(..)) where

import GHC.Generics
import Data.Aeson

import Cards


data Action = Poll
            | PollCard Card
            | StartGame
            | Play Int --play the nth card in one's hand
            | EndTurn
            | Buy Int --buy from the nth pile in the deck
            | Choose Choice
            | NextGame
    deriving (Generic, Show)
instance ToJSON Action
instance FromJSON Action