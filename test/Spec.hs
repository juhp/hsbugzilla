{-# LANGUAGE OverloadedStrings #-}

-- |
module Main (main) where

import Test.Hspec
import Web.Bugzilla.RedHat.Search

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "unit tests" $ do
  describe "search" $ do
    it "eval isNotEmpty" $ do
      evalSearchExpr (isNotEmpty (CustomField "my_field"))
        `shouldBe` [ ("f1", Just "my_field"),
                     ("o1", Just "isnotempty"),
                     ("v1", Just "")
                   ]
    it "eval changedSince" $ do
      evalSearchExpr (changedSince (read "2021-04-01 00:00:00 UTC"))
        `shouldBe` [("chfieldfrom", Just "2021-04-01T00:00:00Z")]
