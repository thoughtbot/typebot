{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Providers.Hayoo where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Aeson (Object, FromJSON(parseJSON), decode, withObject, (.:), (.:?))
import           Data.Aeson.Types (Parser)
import           Data.Maybe (listToMaybe)
import           Data.Text (Text, unpack)
import           Network.HTTP.Conduit
import           Text.Shakespeare.Text
import           Types
import           Utils (removeCommandChars)

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

firstHayooResult :: HayooResults -> Maybe HayooResult
firstHayooResult (HayooResults xs) = listToMaybe xs

hayooUrl :: Text -> String
hayooUrl x = unpack $ [st|http://hayoo.fh-wedel.de/json?query=#{x}|]

hayoo :: (MonadIO m) => Text -> m (Maybe SearchResult)
hayoo f = liftIO $ do
  response <- sequence $ simpleHttp . hayooUrl <$> removeCommandChars f
  return $ translateHayoo <$> (firstHayooResult =<< decode =<< response)

translateHayoo :: HayooResult -> SearchResult
translateHayoo (HayooResult fn (Just ft) fl) = SearchResult (fn ++ " :: " ++ ft) fl
translateHayoo (HayooResult fn _ fl) = SearchResult fn fl
