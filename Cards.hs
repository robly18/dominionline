{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Cards (Card (..), ChoiceFlag (..), Effect (..), Choice (..), CardData,
                effects, cost, reaction, treasure, score, cardData) where


import Data.Aeson
import GHC.Generics

data Card = Copper | Silver | Gold
          | Estate | Dutchy | Province
          | Village | Forge | Lumberjack | Market
          | Remodel | Cellar | Workshop | Moat | Militia | Mine
    deriving (Generic, Show, Eq, Ord)
instance ToJSON Card
instance FromJSON Card

data ChoiceFlag = CFRemodel | CFCellar | CFWorkshop | CFMilitia | CFMine
    deriving (Generic, Show)
instance ToJSON ChoiceFlag
instance FromJSON ChoiceFlag

data Effect = Money Int --Effects to be associated with cards
            | Actions Int
            | Purchases Int
            | Draw Int
            | Action --This isnt quite an effect, but a condition: this card expends one action
            | PlayerChoice ChoiceFlag
            | OtherPlayerChoice ChoiceFlag
    deriving (Generic, Show)
instance ToJSON Effect
instance FromJSON Effect


data Choice = CRemodel Int Int --Card to discard, card to purchase
            | CCellar [Int] --Cards to discard
            | CWorkshop Int --Card to purchase
            | CMilitia [Int] --Cards to discard
            | CMine Int Int --Card to discard, card to purchase
            | SkipChoice --careful not to let players skip choices such as militia!
    deriving (Generic, Show)
instance ToJSON Choice
instance FromJSON Choice

effects :: Card -> [Effect]
effects Copper = [Money 1]
effects Silver = [Money 2]
effects Gold = [Money 3]
effects Village = [Action, Actions 2, Draw 1]
effects Forge = [Action, Draw 3]
effects Lumberjack = [Action, Money 2, Purchases 1]
effects Market = [Action, Money 1, Actions 1, Purchases 1, Draw 1]
effects Remodel = [Action, PlayerChoice CFRemodel]
effects Cellar = [Action, Actions 1, PlayerChoice CFCellar]
effects Workshop = [Action, PlayerChoice CFWorkshop]
effects Moat = [Action, Draw 2]
effects Militia = [Action, Money 2, OtherPlayerChoice CFMilitia]
effects Mine = [Action, PlayerChoice CFMine]
effects _ = []

cost :: Card -> Int
cost Copper = 0
cost Silver = 3
cost Gold = 6
cost Estate = 2
cost Dutchy = 5
cost Province = 8
cost Forge = 4
cost Village = 3
cost Lumberjack = 3
cost Market = 5
cost Remodel = 4
cost Cellar = 2
cost Workshop = 3
cost Moat = 2
cost Militia = 4
cost Mine = 5

reaction :: Card -> Bool
reaction Moat = True
reaction _ = False

treasure :: Card -> Bool
treasure Copper = True
treasure Silver = True
treasure Gold = True
treasure _ = False

score :: Card -> Int --this isnt general enough to deal with gardens. perhaps fold this into effects eventually?
score Estate = 1
score Dutchy = 3
score Province = 6
score _ = 0


type CardData = (String, [Effect], Int, Bool, Bool, Int)
cardData :: Card -> CardData --tells you all the info about the card
cardData c = (show c, effects c, cost c, reaction c, treasure c, score c)
