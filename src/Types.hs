{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Reader (MonadIO, MonadReader, ReaderT)
import           Data.Configurator.Types
import           Data.Aeson (FromJSON(parseJSON), Value(..), object, (.=), (.:))
import           Web.Scotty.Trans (ActionT)
import qualified Data.Text.Lazy as L

data SearchResult = SearchResult { typeString :: String, locationURL :: String } deriving (Show)

instance FromJSON SearchResult where
  parseJSON (Object v) = SearchResult <$> v .: "self" <*> v .: "location"

newtype ResultList = ResultList [SearchResult] deriving (Show)

firstResult :: ResultList -> Maybe SearchResult
firstResult (ResultList (x:_)) = Just x
firstResult _ = Nothing

instance FromJSON ResultList where
  parseJSON (Object v) = ResultList <$> v .: "results"

newtype ConfigM a = ConfigM { runConfigM :: ReaderT Config IO a } deriving (Applicative, Functor, Monad, MonadIO, MonadReader Config)

type TypeBot a = ActionT L.Text ConfigM a
