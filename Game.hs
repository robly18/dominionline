{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Game (newGame, act, State, scrambleState, encode, decode, joinGame,
            Action, RL, GameLog,
            module Control.Monad.Writer) where

{-In this module, try to use as many functions from GameInternal as possible-}
import GameInternal


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
import Actions
import PointedList
import Data.List.PointedList (focus, index, moveTo, fromList)
import Data.List.PointedList.Circular (next, moveN)






actOnEffect :: Effect -> GameState -> RL GameState
actOnEffect (Money i) s = overMoney (+i) s
actOnEffect (Actions i) s = overActions (+i) s
actOnEffect (Purchases i) s = overPurchases (+i) s
actOnEffect (Draw i) s = do let player = s ^. players ^. focus
                            newplayer <- (iterate (>>= draw) (return player)) !! i
                            return $ set (players . focus) newplayer s
actOnEffect (PlayerChoice c) s = (players . focus) (pushChoice c) s
actOnEffect (OtherPlayerChoice c) s = (players . traverse) (\p -> if p == s ^. players ^. focus then return p else pushChoice c p) s

--temporary
actOnEffect Action s = return s


{-actOnEffect Action s = if s ^. players ^. focus ^. actions > 0 then do tell $ return $ PlayerChangeEvent (index $ s ^. players) $ ActionDelta (-1)
                                                                       return $ return $ s & (players . focus . actions) %~ (subtract 1)
                                                                else return Nothing-}

actOnEffects :: [Effect] -> GameState -> RL GameState
actOnEffects = flip (foldM (flip actOnEffect))


data Response = RPoll State
              | RCardData CardData
              | RNull
    deriving (Generic, Show)
makeLenses ''Response

instance ToJSON Response
instance FromJSON Response

act :: (Int, Action) -> State -> RL (State, Response)
act (p, Poll) s = return (s, RPoll $ scrambleState p s)

act (_, PollCard c) s = return (s, RCardData $ cardData c)

act (plr, StartGame) (JoiningState plrs) = case (startGame plr plrs) of
                                                Nothing -> return (JoiningState plrs, RNull)
                                                Just rlgs -> liftM ((,RNull) . GameState) rlgs

act (plr, action) (GameState s@(GS _ _)) = liftM ((,RNull) . checkGameEnd) $
        case fmap (view focus) $ moveTo plr (s ^. players) of
            Nothing -> return s
            Just p -> case p ^. pendingChoices of
                        [] ->   if index (s ^. players) /= plr then return s else
                                case action of
                                    EndTurn -> endTurn s
                                    Buy i -> buyCard s i
                                    Play i -> playCard s i
                                    _ -> return s
                        _ ->    case action of
                                    Choose c -> actOnChoice s plr c
                                    _ -> return s

act (_, NextGame) (EndState _ _) = return (newGame, RNull)

act _ s = return (s, RNull)

checkGameEnd :: GameState -> State
checkGameEnd s = if (length $ filter ((==0) . snd) (s ^. table)) < 3 then GameState s else
                    EndState s (foldl (\l p -> (p ^. playerno, sum $ map score $ p ^. hand ++ p ^.deck ++ p ^. discarded ++ p ^. played):l) [] (s ^. players))


                                       
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
                                                   --todo check that the card can actually be played
                                                   actOnCard card (s & (players . focus) .~ newplayer)

actOnCard :: Card -> GameState -> RL GameState
actOnCard c = actOnEffects (effects c)

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
                    else return $ (return s) >>= (grabStoreCard i)
                                             >>= (overPurchases (subtract 1))
                                             >>= (overMoney (subtract $ cost c)))


