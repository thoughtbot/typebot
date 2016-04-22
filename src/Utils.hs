{-# LANGUAGE OverloadedStrings #-}

module Utils (authorized, requireParameter, lookupParameter, opts) where

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader (ask, lift)
import           Data.Configurator.Types
import           Data.Default.Class (def)
import           Data.Maybe (catMaybes, isJust)
import           Data.Text (splitOn, Text(..), pack)
import           Data.Text.Lazy.Encoding (decodeUtf8)
import           Network.HTTP.Types.Status (Status(..))
import           Network.Wai.Handler.Warp (setPort)
import           System.Environment (getEnv)
import           Types
import           Web.Scotty.Trans (Options, settings, status, body)
import qualified Data.Configurator as C
import qualified Data.Text.Lazy as L

opts :: IO Options
opts = do
  port <- webPort
  return def { settings = setPort port $ settings def }

authorized :: TypeBot () -> TypeBot ()
authorized action = do
  cfg <- lift ask
  rToken <- lookupParameter "token"
  match <- tokenMatches rToken
  if match then action else unauthorized

tokenMatches :: Maybe Text -> TypeBot Bool
tokenMatches (Just t) = do
  cfg <- lift ask
  cToken <- liftIO (C.lookup cfg t :: IO (Maybe Text))
  return $ isJust cToken
tokenMatches' Nothing = return False

requireParameter :: Text -> (Text -> TypeBot ()) -> TypeBot ()
requireParameter name action = do
  p <- lookupParameter name
  maybe badRequest action p

lookupParameter :: Text -> TypeBot (Maybe Text)
lookupParameter name = do
  params <- parseFormEncodedBody . L.toStrict . decodeUtf8 <$> body
  return $ lookup name params

badRequest :: TypeBot ()
badRequest = status $ Status 400 "Bad Request"

unauthorized :: TypeBot ()
unauthorized = status $ Status 401 "Unauthorized"

parseFormEncodedBody :: Text -> [(Text, Text)]
parseFormEncodedBody s = catMaybes $ fmap (arrayToTuple . splitOn "=") (splitOn "&" s)

arrayToTuple :: [Text] -> Maybe (Text, Text)
arrayToTuple [x, y] = Just (x,y)
arrayToTuple _ = Nothing

webPort :: IO Int
webPort = read <$> getEnv "PORT"
