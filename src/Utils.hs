{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Utils (authorized, requireParameter, badRequest, webPort) where

import           Control.Monad.IO.Class (liftIO)
import           Data.Maybe (catMaybes)
import           Data.Text (splitOn, Text(..), pack)
import           Data.Text.Lazy (toStrict)
import           Data.Text.Lazy.Encoding (decodeUtf8)
import           Network.HTTP.Types.Status (Status(..))
import           System.Environment (getEnv)
import qualified Web.Scotty as S

authorized :: S.ActionM () -> S.ActionM ()
authorized action = do
  token <- lookupParameter "token"
  authorized <- liftIO $ withAuthorization token
  case authorized of
    True -> action
    False -> unauthorized

requireParameter :: Text -> (Text -> S.ActionM ()) -> S.ActionM ()
requireParameter name action = do
  p <- lookupParameter name
  maybe badRequest action p

lookupParameter :: Text -> S.ActionM (Maybe Text)
lookupParameter name = do
  params <- parseFormEncodedBody . toStrict . decodeUtf8 <$> S.body
  return $ lookup name params

withAuthorization :: Maybe Text -> IO Bool
withAuthorization (Just token) = getEnv "TYPEBOT_SLACK_TOKEN" >>= return . (== token) . pack
withAuthorization _ = return False

badRequest :: S.ActionM ()
badRequest = S.status $ Status 400 "Bad Request"

unauthorized :: S.ActionM ()
unauthorized = S.status $ Status 401 "Unauthorized"

parseFormEncodedBody :: Text -> [(Text, Text)]
parseFormEncodedBody s = catMaybes $ fmap arrayToTuple $ fmap (splitOn "=") $ splitOn "&" s

arrayToTuple :: [Text] -> Maybe (Text, Text)
arrayToTuple (x:y:[]) = Just (x,y)
arrayToTuple _ = Nothing

webPort :: IO Int
webPort = read <$> getEnv "PORT"
