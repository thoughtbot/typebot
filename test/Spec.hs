{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Main (main) where

import           Test.Hspec
import           Test.Hspec.Wai
import           Test.Hspec.Wai.JSON
import           Data.Aeson (Value(..), object, (.=))

import           Bot (app)

main :: IO ()
main = hspec spec

spec :: Spec
spec = with app $ do
  describe "POST /type" $ do
    describe "when TYPEBOT_SLACK_TOKEN isn't set" $ do
      it "responds with 401" $ do
        post "/type" "" `shouldRespondWith` 401

  describe "GET /" $ do
    it "responds with 200" $ do
      get "/" `shouldRespondWith` 200
