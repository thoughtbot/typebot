{-# LANGUAGE OverloadedStrings #-}

module Bot (runApp) where

import           Control.Monad ((<=<), void)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Reader (MonadReader, ReaderT, ask, lift, runReaderT)
import           Data.Aeson (decode)
import           Data.Configurator as C
import           Data.Configurator.Types (Config)
import           Data.Text (Text(..), unpack, pack, replace, splitOn, splitAt)
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
import           Web.Scotty.Trans (ScottyT, get, post, scottyOptsT, status)
import qualified Data.Text.Lazy as L
import qualified Network.HTTP.Base as U
import qualified Web.Scotty as S

runApp :: IO ()
runApp = do
  c <- C.load [Required "app.cfg"]
  o <- opts
  scottyOptsT o (runIO c) app' where
    runIO :: Config -> ConfigM a -> IO a
    runIO c m = runReaderT (runConfigM m) c

app' :: ScottyT L.Text ConfigM ()
app' = do
  get "/" $ status $ Status 200 "OK"
  post "/type" $ authorized $ requireParameter "text" typeRequest

typeRequest :: Text -> TypeBot ()
typeRequest = maybe badRequest slackRequest <=< hoogle

functionNameToUrl :: Text -> String
functionNameToUrl x = unpack $ mconcat ["https://www.haskell.org/hoogle/?mode=json&hoogle=", x,"&start=1&count=1"]

hoogle :: (MonadIO m) => Text -> m (Maybe SearchResult)
hoogle f = liftIO $ do
  response <- simpleHttp . functionNameToUrl $ removeCommandChars f
  return $ firstResult =<< decode response

slackRequest :: SearchResult -> TypeBot ()
slackRequest s = do
  cfg <- lift ask
  (Just rToken) <- lookupParameter "token"
  liftIO $ do
    slackUrl <- C.require cfg rToken
    request <- parseUrl slackUrl
    let req = request { method = "POST", requestBody = requestBodyLbs $ jsonPayload s }
    manager <- newManager tlsManagerSettings
    void $ httpLbs req manager

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

removeCommandChars :: Text -> Text
removeCommandChars = urlEncode . toHtmlEncodedText . replacePlus . snd . Data.Text.splitAt 5

requestBodyLbs = RequestBodyLBS . encodeUtf8 . L.fromStrict . pack . mconcat

toText = L.toStrict . toLazyText

toHtmlEncodedText = toText . htmlEncodedText . pack . unEscapeString

replacePlus = unpack . replace "+" "%20"

urlEncode = pack . U.urlEncode . unpack

