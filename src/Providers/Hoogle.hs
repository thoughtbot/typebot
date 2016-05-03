{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Providers.Hoogle where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Aeson (Object, FromJSON(parseJSON), decode, withObject, (.:))
import           Data.Aeson.Types (Parser)
import           Data.Maybe (listToMaybe)
import           Data.Text (Text, unpack)
import           Network.HTTP.Conduit
import           Text.Shakespeare.Text
import           Types
import           Utils (removeCommandChars)

data HoogleResult = HoogleResult
  { hoogleType :: String
  , hoogleLocationURL :: String
  } deriving (Show)

instance FromJSON HoogleResult where
  parseJSON = withObject "" parseHoogleResult

parseHoogleResult :: Object -> Parser HoogleResult
parseHoogleResult v = HoogleResult <$> v .: "self" <*> v .: "location"

newtype HoogleResults = HoogleResults [HoogleResult] deriving (Show)

instance FromJSON HoogleResults where
  parseJSON = withObject "" parseHoogleResults

parseHoogleResults :: Object -> Parser HoogleResults
parseHoogleResults v = HoogleResults <$> v .: "results"

firstHoogleResult :: HoogleResults -> Maybe HoogleResult
firstHoogleResult (HoogleResults xs) = listToMaybe xs

hoogleUrl :: Text -> String
hoogleUrl x = unpack $ [st|https://www.haskell.org/hoogle/?mode=json&hoogle=#{x}&start=1&count=1|]

hoogle :: (MonadIO m) => Text -> m (Maybe SearchResult)
hoogle f = liftIO $ do
  response <- simpleHttp . hoogleUrl $ removeCommandChars f
  return $ translateHoogle <$> (firstHoogleResult =<< decode response)

translateHoogle :: HoogleResult -> SearchResult
translateHoogle (HoogleResult ht hlurl) = SearchResult ht hlurl
