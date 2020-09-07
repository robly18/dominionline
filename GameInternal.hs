{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}


module GameInternal (newGame, joinGame, State, encode, decode,
            startGame,
            overMoney, overActions, overPurchases, draw, pushChoice, grabStoreCard,
            endTurn,
            Action, RL, GameLog,
            module Control.Monad.Writer) where

{-This module's purpose: expose primitive functions for modifying game state-}

import Data.Maybe
import Data.Aeson
import Data.List
import Data.Foldable

import Control.Monad
import Control.Monad.Loops
import Control.Monad.Random.Lazy (lift, RandT, StdGen)
import Control.Monad.Writer
import System.Random.Shuffle

import Control.Lens hiding (index, (.=), Choice)

import GHC.Generics

import Cards
import State
import GameLog
import PointedList
import Data.List.PointedList (focus, index, moveTo, fromList)
import Data.List.PointedList.Circular (next, moveN)



type RL = RandT StdGen (Writer GameLog)


joinGame :: State -> Writer GameLog (Maybe Int, State)
joinGame (JoiningState plrs) = do let plrno = length plrs
                                  tell $ return $ JoinEvent plrno
                                  return (Just plrno, JoiningState $ plrs ++ [newPlayer plrno])
joinGame s = return (Nothing, s)

startingTable :: [(Card, Int)] --Temporary!!
startingTable = map (,10) [Copper, Silver, Gold, Estate, Duchy, Province, Forge, Village, Lumberjack, Market, Remodel, Cellar, Workshop, Moat, Militia, Mine]

startGame :: Int -> [Player] -> Maybe (RL GameState)
startGame p plrs = do pointedplrs <- (fromList plrs) >>= moveTo p
                      return $ do tell $ return $ StartGameEvent p
                                  return $ GS pointedplrs startingTable

--All of the following primitives apply to the focused player, as they are the only one who has money and stuff
overMoney :: (Int -> Int) -> GameState -> RL GameState
overMoney f s = do let nm = f $ s ^. players ^. focus ^. money
                   tell $ return $ PlayerChangeEvent $ MoneySet nm
                   return $ set (players . focus . money) nm s

overActions :: (Int -> Int) -> GameState -> RL GameState
overActions f s = do let nm = f $ s ^. players ^. focus ^. actions
                     tell $ return $ PlayerChangeEvent $ ActionsSet nm
                     return $ set (players . focus . actions) nm s

overPurchases :: (Int -> Int) -> GameState -> RL GameState
overPurchases f s = do let nm = f $ s ^. players ^. focus ^. purchases
                       tell $ return $ PlayerChangeEvent $ PurchasesSet nm
                       return $ set (players . focus . purchases) nm s



draw :: Player -> RL Player
draw p = case p ^. deck of
            (c:cs) -> (tell $ return $ DeckChangeEvent (p ^. playerno) $ DCDraw c) >> return ((over hand (++[c]) . set deck cs) p)
            [] -> case p ^. discarded of
                    [] -> return p
                    _ -> (shuffleDiscarded p) >>= draw

        where shuffleDiscarded p = do tell $ return $ DeckChangeEvent (p ^. playerno) DCShuffle
                                      newdeck <- shuffleM (p ^. discarded ++ p ^. deck)
                                      return $ p & discarded .~ [] & deck .~ newdeck

drawN :: Int -> Player -> RL Player
drawN n = (!! n) . (iterate (>>= draw)) . return

discard :: Int -> Player -> RL Player
discard i p = case p ^. hand ^? ix i of
                Nothing -> return p --maybe make an error message eventually?
                Just discarded -> do tell $ return $ DeckChangeEvent (p ^. playerno) (DCDiscard i)
                                     let (start, end) = splitAt i (p ^. hand)
                                     return $
                                       set hand (start ++ tail end) $
                                       over played (discarded:) p

discardAll :: Player -> RL Player --not to be confused with applying discard many times: this sends the discarded cards to "discard" instead of played!
discardAll p = do iterateUntilM (null . view hand) (discard 1) p
                  tell $ return $ DeckChangeEvent (p ^. playerno) DCDiscardPlayed
                  return $
                    over discarded (p ^. played ++) $
                    set played [] p

pushChoice :: ChoiceFlag -> Player -> RL Player
pushChoice cf p = do tell $ return $ PushChoiceEvent (p ^. playerno) cf
                     return $ over pendingChoices (++[cf]) p

endTurn :: GameState -> RL GameState --todo dont allow a player to end turn if there are pending choices
endTurn s = do  tell $ return $ EndTurnEvent
                news <- (players . focus) (drawN 5 >=> discardAll . set actions 1 . set purchases 1 . set money 0) s
                let newnews = over players next news
                return newnews


grabStoreCard :: Int -> GameState -> RL GameState --should there be checking that this is actually possible? atm it just NOOP in that case.
grabStoreCard = undefined



