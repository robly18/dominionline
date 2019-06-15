{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE OverloadedStrings #-}

module Game (newGame, act, State, scrambleState, encode, decode, joinGame,
            Action, RL,
            module Control.Monad.Writer) where


import Data.Maybe
import Data.Aeson
import Data.List
import Data.Foldable

import Data.List.PointedList.Circular (fromList, focus, next, PointedList, moveN, index)
import Data.List.PointedList (focus, prefix, suffix, moveTo)

import Control.Monad
import Control.Monad.Random.Lazy (lift, RandT, StdGen)
import Control.Monad.Writer
import System.Random.Shuffle

import Control.Lens hiding (index, (.=), Choice)

import GHC.Generics



instance (ToJSON a) => ToJSON (PointedList a) where
    toJSON list = object ["index" .= index list, "list" .= (view prefix list ++ [view focus list] ++ view suffix list)]
instance (FromJSON a) => FromJSON (PointedList a) where
    parseJSON (Object v) = moveN <$> (v .: "index") <*> (fromJust <$> (fromList <$> v .: "list")) 
    parseJSON _ = mzero

type RL = RandT StdGen (Writer [String])

data Card = Copper | Silver | Gold
          | Estate | Dutchy | Province
          | Village | Forge | Lumberjack | Market
          | Remodel -- | Cellar | Workshop | Moat | Militia | Mine
    deriving (Generic, Show, Eq, Ord)
instance ToJSON Card
instance FromJSON Card

data ChoiceFlag = CFRemodel
    deriving (Generic, Show)
instance ToJSON ChoiceFlag
instance FromJSON ChoiceFlag

data Choice = CRemodel Int Int --Card to discard, card to purchase
    deriving (Generic, Show)
instance ToJSON Choice
instance FromJSON Choice

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


draw :: Player -> RL Player
draw p = case _deck p of
            (c:cs) -> (lift $ tell ["Drawing one"]) >> return ((over hand (c:) . set deck cs) p)
            [] -> case _discarded p of
                    [] -> (lift $ tell $ ["Ran out of cards"]) >> return p
                    _ -> (lift $ tell $ ["Shuffling discarded"]) >> (shuffleDiscarded p) >>= draw

shuffleDiscarded :: Player -> RL Player
shuffleDiscarded p = do newdeck <- shuffleM (_discarded p ++ _deck p)
                        return $ p {_discarded = [], _deck = newdeck}

discardDraw :: Player -> RL Player
discardDraw p = do lift $ tell $ ["discard drawing"]
                   let pp = p {_hand = [], _played = [], _discarded = _hand p ++ _played p ++ _discarded p}
                   iterate (>>= draw) (return pp) !! 5 --careful. assuming there are at least 5 cards. this might be false later on.

data GameState = GS {_players :: PointedList Player,
                     _table :: [(Card, Int, Int)]} --Card, Amount, Cost
    deriving (Generic, Show)
makeLenses ''GameState

instance ToJSON GameState
instance FromJSON GameState

data State = JoiningState [Player]
           | GameState GameState
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

newGame :: State
newGame = JoiningState []

data Action = Poll
            | StartGame
            | Say String
            | Play Int --play the nth card in one's hand
            | EndTurn
            | Buy Int --buy from the nth pile in the deck
            | Choose Choice
    deriving (Generic, Show)
instance ToJSON Action
instance FromJSON Action

act :: State -> (Int, Action) -> RL State 
act s (_  , Poll) = return s

act (JoiningState plrs) (plr, StartGame) = liftM GameState $ players (traverse discardDraw) $ GS (moveN plr $ fromJust $ fromList plrs)
    [(Copper, 10, 0), (Silver, 10, 3), (Gold, 10, 6),
     (Estate, 10, 2), (Dutchy, 10, 5), (Province, 10, 8),
     (Forge, 10, 4), (Village, 10, 3), (Lumberjack, 10, 3), (Market, 10, 5)]

act s (plr, Say x) = do lift $ tell [show plr ++ ": " ++ x]
                        return s

act (GameState s@(GS _ _)) (plr2, action) = liftM GameState $
        case fmap (view focus) $ moveTo plr2 (s ^. players) of
            Nothing -> return s
            Just p -> case p ^. pendingChoices of
                        [] ->   if index (s ^. players) /= plr2 then return s else
                                case action of
                                    EndTurn -> endTurn s
                                    Buy i -> buyCard s i
                                    Play i -> playCard s i
                                    _ -> return s
                        _ ->    case action of
                                    Choose c -> actOnChoice s plr2 c
                                    _ -> return s

act s _ = return s

endTurn :: GameState -> RL GameState --todo dont allow a player with pending choices to end turn
endTurn s = do  lift $ tell ["Player " ++ show (s ^. players & index) ++ " ends their turn."]
                news <- (players . focus) (discardDraw . set actions 1 . set purchases 1 . set money 0) s
                let newnews = news & players %~ next
                lift $ tell ["It's player " ++ show (newnews ^. players & index) ++"'s turn."]
                return newnews
                                       
extractListElement :: Int -> [a] -> Maybe ([a], a)
extractListElement 0 (x:xs) = Just (xs, x)
extractListElement n (x:xs) = fmap (over _1 (x:)) (extractListElement (n-1) xs)
extractListElement _ [] = Nothing

playCard :: GameState -> Int -> RL GameState
playCard s i = let player = s ^. players ^. focus
                   itshand = player ^. hand in
               case extractListElement i itshand of
                        Nothing -> return s
                        Just (newhand, card) -> do let newplayer = player {_hand = newhand, _played = card : _played player}
                                                   newstate <- actOnCard card (s & (players . focus) .~ newplayer)
                                                   case newstate of Nothing -> return s
                                                                    Just ns -> return ns


data Effect = Money Int --Effects to be associated with cards
            | Actions Int
            | Purchases Int
            | Draw Int
            | Action --This isnt quite an effect, but a condition: this card expends one action
            | PlayerChoice ChoiceFlag

actOnEffect :: Effect -> GameState -> RL (Maybe GameState)
actOnEffect (Money i) = return . return . over (players . focus . money) (+i)
actOnEffect (Actions i) = return . return . over (players . focus . actions) (+i)
actOnEffect (Purchases i) = return . return . over (players . focus . purchases) (+i)
actOnEffect (Draw i) = liftM return . (players . focus) ((!!i) . (iterate (>>= draw)) . return)
actOnEffect Action = \s -> if s ^. players ^. focus ^. actions > 0 then return $ return $ s & (players . focus . actions) %~ (subtract 1) else (lift $ tell ["Can't play this card! Not enough actions."]) >> return Nothing
actOnEffect (PlayerChoice c) = return . return . over (players . focus . pendingChoices) (++[c])

actOnEffects :: [Effect] -> GameState -> RL (Maybe GameState)
actOnEffects [] s = return $ return s
actOnEffects (e:es) s = do mns <- actOnEffect e s
                           case mns of Nothing -> return Nothing
                                       Just ns -> actOnEffects es ns

effects :: Card -> [Effect]
effects Copper = [Money 1]
effects Silver = [Money 2]
effects Gold = [Money 3]
effects Village = [Action, Actions 2, Draw 1]
effects Forge = [Action, Draw 3]
effects Lumberjack = [Action, Money 2, Purchases 1]
effects Market = [Action, Money 1, Actions 1, Purchases 1, Draw 1]
effects Remodel = [Action, PlayerChoice CFRemodel]
effects _ = []

actOnCard :: Card -> GameState -> RL (Maybe GameState)
actOnCard c = ((lift $ tell ["Playing " ++ show c]) >>) . actOnEffects (effects c)

actOnChoice :: GameState -> Int -> Choice -> RL GameState
actOnChoice s p c =
    case preview (element p) (s ^. players) of
        Nothing -> return s
        Just player ->
                    let ss = s & (players . element p . pendingChoices) %~ drop 1 in
                    case (player ^. pendingChoices ^? _head, c) of
                        (Just CFRemodel, CRemodel kc ps) -> return ss --remove the kc'th card from player's deck, buy the ps'th, fail if price doesnt match
                        _ -> return s
                        

buyCard :: GameState -> Int -> RL GameState
buyCard s i = fromMaybe (return s)
                (do (c, amt, cost) <- s ^. table ^? element i
                    let plr = s ^. players ^. focus
                    if amt == 0 then Nothing
                    else if plr ^. purchases == 0 then Nothing
                    else if plr ^. money < cost then Nothing
                    else return $ lift $ tell ["Player " ++ show (index $ _players s) ++ " buys a " ++ show c ++ "."]
                                  >> (return $ s & (players . focus) %~ (over money (subtract $ cost) . over purchases (subtract 1) . over played (c:))
                                                 & (table . element i . _2) %~ (subtract 1)))

joinGame :: State -> Writer [String] (Maybe Int, State)
joinGame (JoiningState plrs) = do let plrno = length plrs
                                  tell ["Player " ++ show plrno ++ " joins."]
                                  return (Just plrno, JoiningState $ plrs ++ [newPlayer plrno])
joinGame s = return (Nothing, s)

