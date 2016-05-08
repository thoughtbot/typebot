{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Bot (runApp) where

import           Control.Monad (void)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Reader (asks, lift, runReaderT)
import           Data.Configurator as C
import           Data.Configurator.Types (Config)
import           Data.Text (Text, unpack)
import           Data.Text.Lazy.Encoding (encodeUtf8)
import           Network.HTTP.Conduit
import           Network.HTTP.Types.Status (Status(..))
import           Providers.Hayoo
import           Providers.Hoogle
import           Text.Shakespeare.Text
import           Types
import           Utils
import           Web.Scotty.Trans (ScottyT, get, post, scottyOptsT, status)
import qualified Data.Text.Lazy as L

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
search Hoogle = hoogle
search Hayoo = hayoo

humanFriendlyUrl :: SearchEngine -> Text -> String
humanFriendlyUrl Hayoo x = unpack $ [st|https://www.haskell.org/hoogle/?hoogle=#{x}&start=1&count=1|]
humanFriendlyUrl Hoogle x = unpack $ [st|http://hayoo.fh-wedel.de/?query=#{x}|]

noResultsSlack :: Text -> TypeBot ()
noResultsSlack t = do
  s <- lift $ asks appSearchEngine
  case (removeCommandChars t) of
    Just t' -> slack $ notFoundPayload s t'
    Nothing -> slack $ parseError

notFoundPayload :: SearchEngine -> Text -> Text
notFoundPayload s t = [st|{ "text": "I couldn't find a matching result, here's where I looked: #{humanFriendlyUrl s t}" }|]

parseError :: Text
parseError = [st|{ "text": "I couldn't parse the your request..." }|]

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

requestBodyLbs :: Text -> RequestBody
requestBodyLbs = RequestBodyLBS . encodeUtf8 . L.fromStrict
