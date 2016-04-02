{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Bot (runApp, app) where

import           Control.Monad.IO.Class
import           Data.Aeson (Value(..), object, (.=))
import           Data.Maybe (catMaybes, fromJust)
import           Data.Text (splitOn, Text(..), unpack, pack)
import           Data.Text.Lazy (toStrict, fromStrict)
import           Data.Text.Lazy.Encoding (decodeUtf8)
import           Network.HTTP.Types.Status (Status(..))
import           Network.Wai (Application)
import qualified Data.Map as M
import qualified Web.Scotty as S

app' :: S.ScottyM ()
app' = do
  S.post "/type" $ do
    res <- parseFormEncodedBody . toStrict . decodeUtf8 <$> S.body
    let x = lookup "text" res
    liftIO $ putStrLn $ fromJust (fmap unpack (fmap removeCommandChars x :: Maybe Text) :: Maybe String)
    S.status $ Status 200 "OK"

parseFormEncodedBody :: Text -> [(Text, Text)]
parseFormEncodedBody s = catMaybes $ fmap arrayToTuple $ fmap (splitOn "=") $ splitOn "&" s

removeCommandChars :: Text -> Text
removeCommandChars t = pack $ snd $ splitAt 5 (unpack t)

arrayToTuple :: [Text] -> Maybe (Text, Text)
arrayToTuple (x:y:[]) = Just (x,y)
arrayToTuple _ = Nothing

app :: IO Application
app = S.scottyApp app'

runApp :: IO ()
runApp = S.scotty 8080 app'
