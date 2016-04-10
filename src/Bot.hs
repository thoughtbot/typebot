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
  S.get "/" $ S.status $ Status 200 "OK"
  S.post "/type" $ authorized $ requireParameter "text" typeRequest

authorized :: S.ActionM () -> S.ActionM ()
authorized f = do
  token <- lookupParameter "token"
  authorized <- liftIO $ withAuthorization token
  case authorized of
    True -> f
    False -> unauthorized

requireParameter :: Text -> (Text -> S.ActionM ()) -> S.ActionM ()
requireParameter t f = do
  p <- lookupParameter t
  maybe badRequest f p

lookupParameter :: Text -> S.ActionM (Maybe Text)
lookupParameter t = do
  params <- parseFormEncodedBody . toStrict . decodeUtf8 <$> S.body
  return $ lookup t params

typeRequest :: Text -> S.ActionM ()
typeRequest t = do
  hoogleRes <- liftIO . simpleHttp . functionNameToUrl $ removeCommandChars t
  maybe badRequest (liftIO . slackRequest) (firstType =<< decode hoogleRes)

withAuthorization :: Maybe Text -> IO Bool
withAuthorization (Just x) = getEnv "TYPEBOT_SLACK_TOKEN" >>= return . (== x) . pack
withAuthorization _ = return False

badRequest :: S.ActionM ()
badRequest = S.status $ Status 400 "Bad Request"

unauthorized :: S.ActionM ()
unauthorized = S.status $ Status 401 "Unauthorized"

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
runApp = (flip S.scotty $ app') =<< webPort

webPort :: IO Int
webPort = read <$> getEnv "PORT"
