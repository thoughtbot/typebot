{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Bot (runApp, app) where

import           Control.Monad.IO.Class
import           Data.Aeson (Value(..), object, (.=))
import           Data.Maybe (catMaybes, fromJust)
import           Data.Text (splitOn, Text(..), unpack, pack, intercalate)
import           Data.Text.Lazy (toStrict, fromStrict)
import           Data.Text.Lazy.Builder (toLazyText)
import           Data.Text.Lazy.Encoding (encodeUtf8, decodeUtf8)
import           Network.HTTP.Types.Status (Status(..))
import           Network.Wai (Application)
import qualified Data.Map as M
import qualified Web.Scotty as S
import           Data.Aeson
import           Network.HTTP.Conduit
import           Network.URI (unEscapeString)
import           HTMLEntities.Decoder (htmlEncodedText)

app' :: S.ScottyM ()
app' = do
  S.post "/type" $ do
    res <- parseFormEncodedBody . toStrict . decodeUtf8 <$> S.body
    hoogleRes <- liftIO . simpleHttp . functionNameToUrl $ removeCommandChars <$> lookup "text" res
    liftIO . slackRequest . firstType $ decode hoogleRes
    S.status $ Status 200 "OK"

parseFormEncodedBody :: Text -> [(Text, Text)]
parseFormEncodedBody s = catMaybes $ fmap arrayToTuple $ fmap (splitOn "=") $ splitOn "&" s

removeCommandChars :: Text -> Text
removeCommandChars t = toHtmlEncodedText . snd . splitAt 5 $ unpack t

toText = toStrict . toLazyText
toHtmlEncodedText = toText . htmlEncodedText . pack . unEscapeString

arrayToTuple :: [Text] -> Maybe (Text, Text)
arrayToTuple (x:y:[]) = Just (x,y)
arrayToTuple _ = Nothing

functionNameToUrl :: Maybe Text -> String
functionNameToUrl (Just x) = unpack $ mconcat ["https://www.haskell.org/hoogle/?mode=json&hoogle=", x,"&start=1&count=1"]
functionNameToUrl Nothing = "https://www.haskell.org/hoogle/?mode=json&hoogle=map&start=1&count=1"

functionNameToUrl' :: Text -> String
functionNameToUrl' x = unpack $ mconcat ["https://www.haskell.org/hoogle/?mode=json&hoogle=", x,"&start=1&count=1"]

app :: IO Application
app = S.scottyApp app'

runApp :: IO ()
runApp = S.scotty 8080 app'

firstType :: Maybe ResultList -> String
firstType Nothing = ""
firstType (Just (ResultList [])) = ""
firstType (Just (ResultList ((SearchResult typeString):_))) = typeString

data SearchResult = SearchResult String deriving (Show)

instance FromJSON SearchResult where
  parseJSON (Object v) = SearchResult <$> v .: "self"

newtype ResultList = ResultList [SearchResult] deriving (Show)

instance FromJSON ResultList where
  parseJSON (Object v) = ResultList <$> v .: "results"

slackPostURL = "READ_FROM_ENV"

slackRequest :: String -> IO ()
slackRequest typeString = do
  request <- parseUrl slackPostURL
  let req = request { method = "POST", requestBody = RequestBodyLBS $ encodeUtf8 $ fromStrict $ pack $ mconcat ["{ \"text\": \"`", typeString, "`\" }"] }
  manager <- newManager tlsManagerSettings
  httpLbs req manager
  return ()
