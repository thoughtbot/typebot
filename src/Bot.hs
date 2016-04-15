{-# LANGUAGE OverloadedStrings #-}

module Bot (runApp) where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad ((<=<))
import           Data.Aeson (decode)
import           Data.Text (Text(..), unpack, pack)
import           Data.Text.Lazy (toStrict, fromStrict)
import           Data.Text.Lazy.Builder (toLazyText)
import           Data.Text.Lazy.Encoding (encodeUtf8)
import           HTMLEntities.Decoder (htmlEncodedText)
import           Network.HTTP.Conduit
import           Network.HTTP.Types.Status (Status(..))
import           Network.URI (unEscapeString)
import           Network.Wai (Application)
import           System.Environment (getEnv)
import           Types
import           Utils
import qualified Web.Scotty as S

app' :: S.ScottyM ()
app' = do
  S.get "/" $ S.status $ Status 200 "OK"
  S.post "/type" $ authorized $ requireParameter "text" typeRequest

typeRequest :: Text -> S.ActionM ()
typeRequest = maybe badRequest slackRequest <=< hoogle

removeCommandChars :: Text -> Text
removeCommandChars t = toHtmlEncodedText . snd . splitAt 5 $ unpack t

toText = toStrict . toLazyText
toHtmlEncodedText = toText . htmlEncodedText . pack . unEscapeString

functionNameToUrl :: Text -> String
functionNameToUrl x = unpack $ mconcat ["https://www.haskell.org/hoogle/?mode=json&hoogle=", x,"&start=1&count=1"]

firstType :: ResultList -> Maybe SearchResult
firstType (ResultList (x:_)) = Just x
firstType _ = Nothing

hoogle :: (MonadIO m) => Text -> m (Maybe SearchResult)
hoogle f = liftIO $ do
  response <- simpleHttp . functionNameToUrl $ removeCommandChars f
  return $ firstType =<< decode response

slackRequest :: (MonadIO m) => SearchResult -> m ()
slackRequest s = liftIO $ do
  slackUrl <- getEnv "TYPEBOT_SLACK_URL"
  request <- parseUrl slackUrl
  let req = request { method = "POST", requestBody = requestBodyLbs $ jsonPayload s }
  manager <- newManager tlsManagerSettings
  httpLbs req manager
  return ()

jsonPayload :: SearchResult -> [String]
jsonPayload s = typePayload s ++ hoogleURL s ++ ["\" }"]

typePayload :: SearchResult -> [String]
typePayload (SearchResult typeString _) =
  [ "{ \"text\":"
  , "\"`"
  , typeString
  , "`"
  , "\n"
  ]

hoogleURL :: SearchResult -> [String]
hoogleURL (SearchResult _ locationURL)
  | not (null locationURL) = ["Hackage docs: ", locationURL]
  | otherwise = []

requestBodyLbs = RequestBodyLBS . encodeUtf8 . fromStrict . pack . mconcat

runApp :: IO ()
runApp = (`S.scotty` app') =<< webPort
