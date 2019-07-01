{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Game (newGame, act, State, scrambleState, encode, decode, joinGame,
            Action, RL,
            module Control.Monad.Writer) where


import Data.Maybe
import Data.Aeson
import Data.List
import Data.Foldable

import Control.Monad
import Control.Monad.Random.Lazy (lift, RandT, StdGen)
import Control.Monad.Writer
import System.Random.Shuffle

import Control.Lens hiding (index, (.=), Choice)

import GHC.Generics

import Cards
import State
import PointedList
import Data.List.PointedList (focus, index, moveTo, fromList)
import Data.List.PointedList.Circular (next, moveN)



type RL = RandT StdGen (Writer [String])



actOnEffect :: Effect -> GameState -> RL (Maybe GameState)
actOnEffect (Money i) = return . return . over (players . focus . money) (+i)
actOnEffect (Actions i) = return . return . over (players . focus . actions) (+i)
actOnEffect (Purchases i) = return . return . over (players . focus . purchases) (+i)
actOnEffect (Draw i) = liftM return . (players . focus) ((!!i) . (iterate (>>= draw)) . return)
actOnEffect Action = \s -> if s ^. players ^. focus ^. actions > 0 then return $ return $ s & (players . focus . actions) %~ (subtract 1) else (tell ["Can't play this card! Not enough actions."]) >> return Nothing
actOnEffect (PlayerChoice c) = return . return . over (players . focus . pendingChoices) (++[c])
actOnEffect (OtherPlayerChoice c) = \s -> return $ return $ s & (players . mapped) %~
                (\p -> if p ^. playerno == index (s ^. players) || any reaction (p ^. hand) then p else p & pendingChoices %~ (++[c]))

actOnEffects :: [Effect] -> GameState -> RL (Maybe GameState)
actOnEffects [] s = return $ return s
actOnEffects (e:es) s = do mns <- actOnEffect e s
                           case mns of Nothing -> return Nothing
                                       Just ns -> actOnEffects es ns

draw :: Player -> RL Player
draw p = case p ^. deck of
            (c:cs) -> (tell ["Drawing one"]) >> return ((over hand (c:) . set deck cs) p)
            [] -> case p ^. discarded of
                    [] -> (tell $ ["Ran out of cards"]) >> return p
                    _ -> (tell $ ["Shuffling discarded"]) >> (shuffleDiscarded p) >>= draw

shuffleDiscarded :: Player -> RL Player
shuffleDiscarded p = do newdeck <- shuffleM (p ^. discarded ++ p ^. deck)
                        return $ p  & discarded .~ [] & deck .~ newdeck

discardDraw :: Player -> RL Player
discardDraw p = do tell $ ["discard drawing"]
                   let pp = p & hand .~ [] & played .~ [] & discarded .~ p ^. hand ++ p ^. played ++ p ^. discarded
                   iterate (>>= draw) (return pp) !! 5 --careful. assuming there are at least 5 cards. this might be false later on.


data Response = RPoll State
              | RCardData CardData
              | RNull
    deriving (Generic, Show)
makeLenses ''Response

instance ToJSON Response
instance FromJSON Response

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

act :: State -> (Int, Action) -> RL (State, Response)
act s (p, Poll) = return (s, RPoll $ scrambleState p s)

act s (_, PollCard c) = return (s, RCardData $ cardData c)

act (JoiningState plrs) (plr, StartGame) = liftM ((,RNull) . GameState) $ players (traverse discardDraw) $ GS (moveN plr $ fromJust $ fromList plrs)
    (map (,10) [Copper, Silver, Gold, Estate, Duchy, Province, Forge, Village, Lumberjack, Market, Remodel, Cellar, Workshop, Moat, Militia, Mine])

act s (plr, Say x) = do tell [show plr ++ ": " ++ x]
                        return (s, RNull)

act (GameState s@(GS _ _)) (plr2, action) = liftM ((,RNull) . checkGameEnd) $
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

act (EndState _ _) (_, NextGame) = return (newGame, RNull)

act s _ = return (s, RNull)

checkGameEnd :: GameState -> State
checkGameEnd s = if (length $ filter ((==0) . snd) (s ^. table)) < 3 then GameState s else
                    EndState s (foldl (\l p -> (p ^. playerno, sum $ map score $ p ^. hand ++ p ^.deck ++ p ^. discarded ++ p ^. played):l) [] (s ^. players))

endTurn :: GameState -> RL GameState --todo dont allow a player with pending choices to end turn
endTurn s = do  tell ["Player " ++ show (s ^. players & index) ++ " ends their turn."]
                news <- (players . focus) (discardDraw . set actions 1 . set purchases 1 . set money 0) s
                let newnews = news & players %~ next
                tell ["It's player " ++ show (newnews ^. players & index) ++"'s turn."]
                return newnews
                                       
extractListElement :: Int -> [a] -> Maybe ([a], a)
extractListElement 0 (x:xs) = Just (xs, x)
extractListElement n (x:xs) = fmap (over _1 (x:)) (extractListElement (n-1) xs)
extractListElement _ [] = Nothing

extractListElements :: [Int] -> [a] -> ([a], [a])
extractListElements ixs = over both (map snd) . partition ((`elem` ixs) . fst) . zip [0..]

playCard :: GameState -> Int -> RL GameState
playCard s i = let player = s ^. players ^. focus
                   itshand = player ^. hand in
               case extractListElement i itshand of
                        Nothing -> return s
                        Just (newhand, card) -> do let newplayer = player & hand .~ newhand & played .~ card : player ^. played
                                                   newstate <- actOnCard card (s & (players . focus) .~ newplayer)
                                                   case newstate of Nothing -> return s
                                                                    Just ns -> return ns

actOnCard :: Card -> GameState -> RL (Maybe GameState)
actOnCard c = ((tell ["Playing " ++ show c]) >>) . actOnEffects (effects c)

actOnChoice :: GameState -> Int -> Choice -> RL GameState
actOnChoice s p c =
    case preview (element p) (s ^. players) of
        Nothing -> return s
        Just player ->
                    let ss = s & (players . element p . pendingChoices) %~ drop 1 in
                    case (player ^. pendingChoices ^? _head, c) of
                        (Just CFRemodel, CRemodel kc ps) -> return $ fromMaybe s
                                (do (newhand, discarded) <- extractListElement kc $ player ^. hand
                                    (bought, amt) <- s ^. table ^? element ps
                                    if cost bought <= cost discarded + 2 && amt > 0 then
                                        return $ ss & (players . element p) %~ (set hand newhand . over played (bought:))
                                                    & (table . element ps . _2) %~ (subtract 1)
                                    else Nothing)
                        (Just CFCellar, CCellar cards) ->
                                (do let (removed, kept) = extractListElements cards (player ^. hand)
                                    ss & (players . element p) ((!! length removed) . (iterate (>>= draw)) . return . set hand kept . over played (removed++)))
                        (Just CFWorkshop, CWorkshop bc) -> return $ fromMaybe s
                                (do (bought, amt) <- s ^. table ^? element bc
                                    if cost bought <= 4 && amt > 0 then
                                        return $ ss & (players . element p . played) %~ (bought:)
                                                    & (table . element bc . _2) %~ (subtract 1)
                                    else Nothing)
                        (Just CFMilitia, CMilitia cards) -> return $ fromMaybe s
                                (do let (removed, kept) = extractListElements cards (player ^. hand)
                                    if length kept <= 3 then
                                        return $ ss & (players . (element p)) %~ set hand kept . over played (removed++)
                                    else Nothing)
                        (Just CFMilitia, SkipChoice) -> return s --no skipping your duties!
                        (Just CFMine, CMine kc ps) -> return $ fromMaybe s
                                (do (newhand, discarded) <- extractListElement kc $ player ^. hand
                                    (bought, amt) <- s ^. table ^? element ps
                                    if treasure bought && treasure discarded && cost bought <= cost discarded + 3 && amt > 0 then
                                        return $ ss & (players . element p . hand) .~ (bought:newhand)
                                                    & (table . element ps . _2) %~ (subtract 1)
                                    else Nothing)
                        (_, SkipChoice) -> return ss --careful not to allow this for eg militia
                        _ -> return s
                        

buyCard :: GameState -> Int -> RL GameState
buyCard s i = fromMaybe (return s)
                (do (c, amt) <- s ^. table ^? element i
                    let plr = s ^. players ^. focus
                    if amt == 0 then Nothing
                    else if plr ^. purchases == 0 then Nothing
                    else if plr ^. money < cost c then Nothing
                    else return $ tell ["Player " ++ show (index $ s ^. players) ++ " buys a " ++ show c ++ "."]
                                  >> (return $ s & (players . focus) %~ (over money (subtract $ cost c) . over purchases (subtract 1) . over played (c:))
                                                 & (table . element i . _2) %~ (subtract 1)))

joinGame :: State -> Writer [String] (Maybe Int, State)
joinGame (JoiningState plrs) = do let plrno = length plrs
                                  tell ["Player " ++ show plrno ++ " joins."]
                                  return (Just plrno, JoiningState $ plrs ++ [newPlayer plrno])
joinGame s = return (Nothing, s)

