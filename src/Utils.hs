{-# LANGUAGE OverloadedStrings #-}

module Utils (removeCommandChars, authorized, requireParameter, lookupParameter, opts) where

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader (ask, lift)
import           Data.Default.Class (def)
import           Data.Maybe (catMaybes, isJust, listToMaybe)
import           Data.Text (splitOn, splitAt, unpack, pack, replace, Text)
import           Data.Text.Internal.Builder(Builder)
import           Data.Text.Lazy.Builder (toLazyText)
import           Data.Text.Lazy.Encoding (decodeUtf8)
import           HTMLEntities.Decoder (htmlEncodedText)
import           Network.HTTP.Types.Status (Status(..))
import           Network.URI (unEscapeString)
import           Network.Wai.Handler.Warp (setPort)
import           System.Environment (getEnv)
import           Types
import           Web.Scotty.Trans (Options, settings, status, body)
import qualified Data.Configurator as C
import qualified Data.Text.Lazy as L
import qualified Network.HTTP.Base as U
import qualified Text.ParserCombinators.ReadP as P

opts :: IO Options
opts = do
  port <- webPort
  return def { settings = setPort port $ settings def }

authorized :: TypeBot () -> TypeBot ()
authorized action = do
  rToken <- lookupParameter "token"
  match <- tokenMatches rToken
  if match then action else unauthorized

tokenMatches :: Maybe Text -> TypeBot Bool
tokenMatches (Just t) = do
  (AppConfig cfg _) <- lift ask
  cToken <- liftIO (C.lookup cfg t :: IO (Maybe Text))
  return $ isJust cToken
tokenMatches Nothing = return False

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

removeCommandChars :: Text -> Maybe Text
removeCommandChars t = urlEncode . toHtmlEncodedText . replacePlus <$> (parsedCommand' t)
  where
    parsedCommand' t = pack . snd <$> parsedCommand t
    parsedCommand t = listToMaybe $ (P.readP_to_S $ P.string "%3At+") $ unpack t

toText :: Builder -> Text
toText = L.toStrict . toLazyText

toHtmlEncodedText :: String -> Text
toHtmlEncodedText = Utils.toText . htmlEncodedText . pack . unEscapeString

replacePlus :: Text -> String
replacePlus = unpack . replace "+" "%20"

urlEncode :: Text -> Text
urlEncode = pack . U.urlEncode . unpack
