{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Bot (runApp, app) where

import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson (decode)
import           Data.Maybe (catMaybes)
import           Data.Text (splitOn, Text(..), unpack, pack)
import           Data.Text.Lazy (toStrict, fromStrict)
import           Data.Text.Lazy.Builder (toLazyText)
import           Data.Text.Lazy.Encoding (encodeUtf8, decodeUtf8)
import           HTMLEntities.Decoder (htmlEncodedText)
import           Network.HTTP.Conduit
import           Network.HTTP.Types.Status (Status(..))
import           Network.URI (unEscapeString)
import           Network.Wai (Application)
import           System.Environment (getEnv)
import           Types
import qualified Data.Map as M
import qualified Web.Scotty as S

app' :: S.ScottyM ()
app' = do
  S.post "/type" $ do
    res <- parseFormEncodedBody . toStrict . decodeUtf8 <$> S.body
    case (removeCommandChars <$> lookup "text" res) of
      Just x -> do
        hoogleRes <- liftIO . simpleHttp . functionNameToUrl $ x
        maybe badRequest (liftIO . slackRequest) (firstType =<< decode hoogleRes)
      Nothing -> badRequest

badRequest :: S.ActionM ()
badRequest = S.status $ Status 403 "Malformed"

parseFormEncodedBody :: Text -> [(Text, Text)]
parseFormEncodedBody s = catMaybes $ fmap arrayToTuple $ fmap (splitOn "=") $ splitOn "&" s

removeCommandChars :: Text -> Text
removeCommandChars t = toHtmlEncodedText . snd . splitAt 5 $ unpack t

toText = toStrict . toLazyText
toHtmlEncodedText = toText . htmlEncodedText . pack . unEscapeString

arrayToTuple :: [Text] -> Maybe (Text, Text)
arrayToTuple (x:y:[]) = Just (x,y)
arrayToTuple _ = Nothing

functionNameToUrl :: Text -> String
functionNameToUrl x = unpack $ mconcat ["https://www.haskell.org/hoogle/?mode=json&hoogle=", x,"&start=1&count=1"]

firstType :: ResultList -> Maybe String
firstType (ResultList (x:_)) = Just $ typeString x
firstType _ = Nothing

slackRequest :: String -> IO ()
slackRequest typeString = do
  slackUrl <- getEnv "TYPEBOT_SLACK_URL"
  request <- parseUrl slackUrl
  let req = request { method = "POST", requestBody = requestBodyLbs  ["{ \"text\": \"`", typeString, "`\" }"] }
  manager <- newManager tlsManagerSettings
  httpLbs req manager
  return ()

requestBodyLbs xs = RequestBodyLBS . encodeUtf8 . fromStrict . pack . mconcat $ xs

app :: IO Application
app = S.scottyApp app'

runApp :: IO ()
runApp = S.scotty 8080 app'
