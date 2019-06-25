{-# LANGUAGE OverloadedStrings #-}

module PointedList where

import Data.List.PointedList (PointedList, focus, index, prefix, suffix, fromList, moveN)
import Data.Maybe
import Data.Aeson (ToJSON, toJSON, FromJSON, parseJSON, Value (Object), object, (.=), (.:))

import Control.Monad

import Control.Lens (view)

instance (ToJSON a) => ToJSON (PointedList a) where
    toJSON list = object ["index" .= index list, "list" .= (view prefix list ++ [view focus list] ++ view suffix list)]
instance (FromJSON a) => FromJSON (PointedList a) where
    parseJSON (Object v) = do ix <- v .: "index"
                              ls <- v .: "list"
                              case (fromList ls) >>= moveN ix of Nothing -> mzero
                                                                 Just l -> return l
    parseJSON _ = mzero
