{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
module Main (main) where

import GHC.Generics (Generic)
import Test.Hspec
import Web.Bugzilla.RedHat (BugFields (getFields))
import Web.Bugzilla.RedHat.Search

main :: IO ()
main = hspec spec

-- | The data type containing the included fields
data BugWithScore = BugWithScore
  { bugId :: Int,
    bugSummary :: String,
    bugPmScore :: Int
  }
  deriving (Show, Eq, Generic)

instance BugFields BugWithScore

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

    it "eval changedRange" $ do
      evalSearchExpr
        ( changedRange
            (read "2021-04-01 00:00:00 UTC")
            (read "2021-04-13 00:00:00 UTC")
        )
        `shouldBe` [ -- TODO: fix unused f1 and f4 query args
                     ("f1", Just "OP"),
                     ("f4", Just "CP"),
                     ("chfieldfrom", Just "2021-04-01T00:00:00Z"),
                     ("chfieldto", Just "2021-04-13T00:00:00Z")
                   ]
    it "derive include_fields" $ do
      getFields (BugWithScore 42 "a bug" 9001)
        `shouldBe` ["bugId", "bugSummary", "bugPmScore"]
