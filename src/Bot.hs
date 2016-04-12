{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Bot (runApp) where

import           Control.Monad.IO.Class (liftIO)
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
typeRequest func = do
  hoogleRes <- liftIO . simpleHttp . functionNameToUrl $ removeCommandChars func
  maybe badRequest (liftIO . slackRequest) (firstType =<< decode hoogleRes)

removeCommandChars :: Text -> Text
removeCommandChars t = toHtmlEncodedText . snd . splitAt 5 $ unpack t

toText = toStrict . toLazyText
toHtmlEncodedText = toText . htmlEncodedText . pack . unEscapeString

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

runApp :: IO ()
runApp = (flip S.scotty $ app') =<< webPort
