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
import Data.List.PointedList (focus, prefix, suffix)

import Control.Monad
import Control.Monad.Random.Lazy (lift, RandT, StdGen)
import Control.Monad.Writer
import System.Random.Shuffle

import Control.Lens hiding (index, (.=))

import GHC.Generics



instance (ToJSON a) => ToJSON (PointedList a) where
    toJSON list = object ["index" .= index list, "list" .= (view prefix list ++ [view focus list] ++ view suffix list)]
instance (FromJSON a) => FromJSON (PointedList a) where
    parseJSON (Object v) = moveN <$> (v .: "index") <*> (fromJust <$> (fromList <$> v .: "list")) 
    parseJSON _ = mzero

type RL = RandT StdGen (Writer [String])

data Card = Copper | Victory | Village | Forge --todo
    deriving (Generic, Show, Eq, Ord)
instance ToJSON Card
instance FromJSON Card

data Player = Player { _playerno :: Int,
                       _hand :: [Card],
                       _deck :: [Card],
                       _discarded :: [Card],
                       _played :: [Card],
                       _actions :: Int,
                       _money :: Int }
    deriving (Generic, Show)
instance ToJSON Player
instance FromJSON Player
instance Eq Player where
    p1 == p2 = _playerno p1 == _playerno p2

makeLenses ''Player

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

data State = JoiningState [Player]
           | GameState {_players :: PointedList Player,
                        _table :: [Card]}
    deriving (Generic, Show)

makeLenses ''State

scrambleState :: Int -> State -> State
scrambleState plr s = case s of
    JoiningState _ -> s
    GameState _ _ -> s & players %~ fmap plrscramble
        where plrscramble p = if p ^. playerno == plr then
                                p & deck %~ sort
                              else
                                let newdeck = p ^. hand ++ p ^. deck & sort in p & deck .~ newdeck & hand .~ []

newGame :: State
newGame = JoiningState []

instance ToJSON State
instance FromJSON State

data Action = Poll
            | StartGame
            | Say String
            | Play Int --play the nth card in one's hand
            | EndTurn
            | Buy
    deriving (Generic, Show)
instance ToJSON Action
instance FromJSON Action

act :: State -> (Int, Action) -> RL State --use maybe
act s (_  , Poll) = return s

act (JoiningState plrs) (plr, StartGame) = return $ GameState (moveN plr $ fromJust $ fromList plrs) [Forge, Village, Copper, Copper, Copper, Victory, Victory, Victory]

act s (plr, Say x) = do lift $ tell [show plr ++ ": " ++ x]
                        return s

act s@(GameState plrs _) (plr2, action) = if index plrs /= plr2 then return s else
        case action of
            EndTurn -> nextPlayer s
            Buy -> buyCard s
            Play i -> playCard s i
            _ -> return s

act s _ = return s

nextPlayer :: State -> RL State
nextPlayer (GameState plrs stack) = do lift $ tell ["Player " ++ show (index plrs) ++ " ends their turn."]
                                       newplrs <- traverseOf focus (discardDraw . set actions 1 . set money 0) $ newplrs
                                       let newnewplrs = next plrs
                                       lift $ tell ["It's player " ++ show (index newnewplrs) ++"'s turn."]
                                       return $ GameState newnewplrs stack
                                       
extractListElement :: Int -> [a] -> Maybe ([a], a)
extractListElement 0 (x:xs) = Just (xs, x)
extractListElement n (x:xs) = fmap (over _1 (x:)) (extractListElement (n-1) xs)
extractListElement _ [] = Nothing

playCard :: State -> Int -> RL State
playCard s@(GameState plrs stack) i =
    fromMaybe (return s)
                (do let player = view focus plrs
                    let itshand = _hand player
                    (newhand, card) <- extractListElement i itshand
                    let newplayer = player {_hand = newhand, _played = card : _played player}
                    let newplrs = set focus newplayer plrs
                    newstate <- actOnCard card (s {_players = newplrs})
                    return (lift (tell ["Player " ++ show (index plrs) ++ " played " ++ show card])
                            >> newstate))

action :: (State -> RL State) -> State -> Maybe (RL State)
action a s = if s ^?! players ^. focus ^. actions == 0 then Nothing else return $ (a $ s & (players . focus . actions) %~ (subtract 1))

actOnCard :: Card -> State -> Maybe (RL State)
actOnCard Copper = return .return . over (players . focus . money) (+1)
actOnCard Village = action $ (players . focus) (draw . (over actions (+2)))
actOnCard Forge = action $ (players . focus) ((!!3) . (iterate (>>= draw)) . return)
actOnCard _ =  const Nothing

buyCard :: State -> RL State
buyCard s = do  lift $ tell ["Player " ++ show (index $ _players s) ++ " buys a card."]
                let plrs = s ^?! players
                let plr = plrs ^. focus
                if plr ^. money >= 1 then
                    case s ^. table of
                        (c:cs) -> return $ s & (players . focus) %~ (over money (subtract 1) . over played (c:))
                                             & table .~ cs
                        [] -> return s
                else do
                    return s

joinGame :: State -> Writer [String] (Maybe Int, State)
joinGame (JoiningState plrs) = do let plrno = length plrs
                                  tell ["Player " ++ show plrno ++ " joins."]
                                  return (Just plrno, JoiningState $ plrs ++ [newPlayer $ plrno])
joinGame s = return (Nothing, s)

newPlayer :: Int -> Player
newPlayer i = Player i [Copper, Copper, Victory, Copper, Copper] [] [] [] 1 0

