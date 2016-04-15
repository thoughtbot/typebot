{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Types where

import           Data.Aeson (FromJSON(parseJSON), Value(..), object, (.=), (.:))

data SearchResult = SearchResult { typeString :: String, locationURL :: String } deriving (Show)

instance FromJSON SearchResult where
  parseJSON (Object v) = SearchResult <$> v .: "self" <*> v .: "location"

newtype ResultList = ResultList [SearchResult] deriving (Show)

instance FromJSON ResultList where
  parseJSON (Object v) = ResultList <$> v .: "results"
