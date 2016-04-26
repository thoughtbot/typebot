{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Bot (runApp) where

import           Control.Monad (void)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Reader (asks, lift, runReaderT)
import           Data.Aeson (decode)
import           Data.Configurator as C
import           Data.Configurator.Types (Config)
import           Data.Text (Text, unpack, pack, replace, splitAt)
import           Data.Text.Internal.Builder(Builder)
import           Data.Text.Lazy.Builder (toLazyText)
import           Data.Text.Lazy.Encoding (encodeUtf8)
import           HTMLEntities.Decoder (htmlEncodedText)
import           Network.HTTP.Conduit
import           Network.HTTP.Types.Status (Status(..))
import           Network.URI (unEscapeString)
import           Text.Shakespeare.Text
import           Types
import           Utils
import           Web.Scotty.Trans (ScottyT, get, post, scottyOptsT, status)
import qualified Data.Text.Lazy as L
import qualified Network.HTTP.Base as U

runApp :: IO ()
runApp = do
  c <- C.load [Required "app.cfg"]
  s <- engine <$> C.lookup c "engine"
  o <- opts
  scottyOptsT o (runIO c s) app' where
    runIO :: Config -> SearchEngine -> ConfigM a -> IO a
    runIO c s m = runReaderT (runConfigM m) $ AppConfig c s

engine :: Maybe Text -> SearchEngine
engine (Just "hayoo") = Hayoo
engine _ = Hoogle

app' :: ScottyT L.Text ConfigM ()
app' = do
  get "/" $ status $ Status 200 "OK"
  post "/type" $ authorized $ requireParameter "text" typeRequest

typeRequest :: Text -> TypeBot ()
typeRequest f = do
  s <- lift $ asks appSearchEngine
  result <- search s $ f
  maybe (noResultsSlack f) slackRequest result

search :: (MonadIO m) => SearchEngine -> (Text -> m (Maybe SearchResult))
search Hayoo = hayoo
search Hoogle = hoogle

functionNameToUrl :: Text -> String
functionNameToUrl x = unpack $ mconcat [baseUrl, "?mode=json&hoogle=", x,"&start=1&count=1"]

humanFriendlyUrl :: Text -> String
humanFriendlyUrl x = unpack $ mconcat [baseUrl, "?hoogle=", x,"&start=1&count=1"]

baseUrl :: Text
baseUrl = "https://www.haskell.org/hoogle/"

hayooUrl :: Text -> String
hayooUrl x = unpack $ [st|http://hayoo.fh-wedel.de/json?query=#{x}|]

hoogle :: (MonadIO m) => Text -> m (Maybe SearchResult)
hoogle f = liftIO $ do
  response <- simpleHttp . functionNameToUrl $ removeCommandChars f
  return $ firstResult =<< decode response

hayoo :: (MonadIO m) => Text -> m (Maybe SearchResult)
hayoo f = liftIO $ do
  response <- simpleHttp . hayooUrl $ removeCommandChars f
  return $ translateHayoo <$> (firstHayooResult =<< decode response)

translateHayoo :: HayooResult -> SearchResult
translateHayoo (HayooResult fn (Just ft) fl) = SearchResult (fn ++ " :: " ++ ft) fl
translateHayoo (HayooResult fn _ fl) = SearchResult fn fl

noResultsSlack :: Text -> TypeBot ()
noResultsSlack = slack . notFoundPayload

notFoundPayload :: Text -> Text
notFoundPayload t = [st|{ "text": "I couldn't find a matching result, here's where I looked: #{humanFriendlyUrl $ removeCommandChars t}" }|]

slackRequest :: SearchResult -> TypeBot ()
slackRequest = slack . typePayload

slack :: Text -> TypeBot ()
slack s = do
  cfg <- lift $ asks appConfig
  (Just rToken) <- lookupParameter "token"
  liftIO $ do
    slackUrl <- C.require cfg rToken
    request <- parseUrl slackUrl
    manager <- newManager tlsManagerSettings
    void $ httpLbs (request { method = "POST", requestBody = requestBodyLbs s }) manager

typePayload :: SearchResult -> Text
typePayload (SearchResult ts lurl) = [st|{ "text": "`#{ts}`\nHackage docs: #{lurl}" }|]

removeCommandChars :: Text -> Text
removeCommandChars = urlEncode . toHtmlEncodedText . replacePlus . snd . Data.Text.splitAt 5

requestBodyLbs :: Text -> RequestBody
requestBodyLbs = RequestBodyLBS . encodeUtf8 . L.fromStrict

toText :: Builder -> Text
toText = L.toStrict . toLazyText

toHtmlEncodedText :: String -> Text
toHtmlEncodedText = Bot.toText . htmlEncodedText . pack . unEscapeString

replacePlus :: Text -> String
replacePlus = unpack . replace "+" "%20"

urlEncode :: Text -> Text
urlEncode = pack . U.urlEncode . unpack
