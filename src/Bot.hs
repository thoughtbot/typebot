{-# LANGUAGE OverloadedStrings #-}

module Bot (runApp) where

import           Control.Monad ((<=<), void)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Reader (MonadReader, ReaderT, ask, lift, runReaderT)
import           Data.Aeson (decode)
import           Data.Configurator as C
import           Data.Configurator.Types (Config)
import           Data.Default.Class (def)
import           Data.Text (Text(..), unpack, pack)
import           Data.Text.Lazy.Builder (toLazyText)
import           Data.Text.Lazy.Encoding (encodeUtf8)
import           HTMLEntities.Decoder (htmlEncodedText)
import           Network.HTTP.Base (urlEncode)
import           Network.HTTP.Conduit
import           Network.HTTP.Types.Status (Status(..))
import           Network.URI (unEscapeString)
import           Network.Wai (Application)
import           Network.Wai.Handler.Warp (setPort)
import           System.Environment (getEnv)
import           Types
import           Utils
import           Web.Scotty.Trans (ScottyT, Options, get, post, scottyOptsT, status, settings)
import qualified Data.Text.Lazy as L
import qualified Web.Scotty as S

app' :: ScottyT L.Text ConfigM ()
app' = do
  get "/" $ status $ Status 200 "OK"
  post "/type" $ authorized $ requireParameter "text" typeRequest

typeRequest :: Text -> TypeBot ()
typeRequest = maybe badRequest slackRequest <=< hoogle

removeCommandChars :: Text -> Text
removeCommandChars t = toHtmlEncodedText . urlEncode . snd . splitAt 5 $ unpack t

toText = L.toStrict . toLazyText
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

requestBodyLbs = RequestBodyLBS . encodeUtf8 . L.fromStrict . pack . mconcat

opts :: IO Options
opts = do
  port <- webPort
  return def { settings = setPort port $ settings def }

runApp :: IO ()
runApp = do
  c <- C.load [Required "app.cfg"]
  o <- opts
  scottyOptsT o (runIO c) app' where
    runIO :: Config -> ConfigM a -> IO a
    runIO c m = runReaderT (runConfigM m) c
