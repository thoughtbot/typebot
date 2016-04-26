{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types where

import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Reader (MonadReader, ReaderT)
import           Data.Aeson (Object, FromJSON(parseJSON), withObject, (.:), (.:?))
import           Data.Aeson.Types (Parser)
import           Data.Configurator.Types
import           Data.Maybe (listToMaybe)
import           Web.Scotty.Trans (ActionT)
import qualified Data.Text.Lazy as L

data HayooResult = HayooResult
  { fname :: String
  , ftype :: Maybe String
  , flocation :: String
  } deriving (Show)

instance FromJSON HayooResult where
  parseJSON = withObject "" parseHayooResult

parseHayooResult :: Object -> Parser HayooResult
parseHayooResult v = HayooResult <$> v .: "resultName" <*> v .:? "resultSignature" <*> v .: "resultUri"

newtype HayooResults = HayooResults [HayooResult] deriving (Show)

instance FromJSON HayooResults where
  parseJSON = withObject "" parseHayooResults

parseHayooResults :: Object -> Parser HayooResults
parseHayooResults v = HayooResults <$> v .: "result"

data SearchResult = SearchResult { typeString :: String, locationURL :: String } deriving (Show)

instance FromJSON SearchResult where
  parseJSON = withObject "" parseSearchResult

parseSearchResult :: Object -> Parser SearchResult
parseSearchResult v = SearchResult <$> v .: "self" <*> v .: "location"

newtype ResultList = ResultList [SearchResult] deriving (Show)

data SearchEngine = Hayoo | Hoogle deriving (Show)

firstResult :: ResultList -> Maybe SearchResult
firstResult (ResultList xs) = listToMaybe xs

firstHayooResult :: HayooResults -> Maybe HayooResult
firstHayooResult (HayooResults xs) = listToMaybe xs

instance FromJSON ResultList where
  parseJSON = withObject "" parseResultList

parseResultList :: Object -> Parser ResultList
parseResultList v = ResultList <$> v .: "results"

data AppConfig = AppConfig { appConfig :: Config, appSearchEngine :: SearchEngine }

newtype ConfigM a = ConfigM { runConfigM :: ReaderT AppConfig IO a } deriving (Applicative, Functor, Monad, MonadIO, MonadReader AppConfig)

type TypeBot a = ActionT L.Text ConfigM a
