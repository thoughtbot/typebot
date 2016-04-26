{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Reader (MonadIO, MonadReader, ReaderT)
import           Data.Aeson (FromJSON(parseJSON), Value(..), object, (.=), (.:), (.:?))
import           Data.Configurator.Types
import           Web.Scotty.Trans (ActionT)
import qualified Data.Text.Lazy as L

data SearchResult = SearchResult { typeString :: String, locationURL :: String } deriving (Show)

instance FromJSON SearchResult where
  parseJSON (Object v) = SearchResult <$> v .: "self" <*> v .: "location"

newtype ResultList = ResultList [SearchResult] deriving (Show)

data SearchEngine = Hoogle deriving (Show)

firstResult :: ResultList -> Maybe SearchResult
firstResult (ResultList xs) = safeHead xs

safeHead :: [a] -> Maybe a
safeHead (x:_) = Just x
safeHead _ = Nothing

instance FromJSON ResultList where
  parseJSON (Object v) = ResultList <$> v .: "results"

data AppConfig = AppConfig Config SearchEngine

newtype ConfigM a = ConfigM { runConfigM :: ReaderT AppConfig IO a } deriving (Applicative, Functor, Monad, MonadIO, MonadReader AppConfig)

type TypeBot a = ActionT L.Text ConfigM a
