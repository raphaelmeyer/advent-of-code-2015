{-# LANGUAGE OverloadedStrings #-}

module Day08.MatchsticksSpec where

import qualified Data.Text as Text
import qualified Day08.Matchsticks as Matchsticks
import Test.Hspec

exampleInput :: [Text.Text]
exampleInput =
  [ "\"\"",
    "\"abc\"",
    "\"aaa\\\"aaa\"",
    "\"\\x27\""
  ]

spec :: Spec
spec = do
  describe "difference of characters of code and characters in memory" $ do
    it "calculates the difference" $ do
      Matchsticks.diffCodeMemory exampleInput `shouldBe` 12

  describe "count characters of code" $ do
    it "counts number of characters of code in a single line" $ do
      Matchsticks.charsOfCode "\"\"" `shouldBe` 2
      Matchsticks.charsOfCode "\"abc\"" `shouldBe` 5
      Matchsticks.charsOfCode "\"aaa\\\"aaa\"" `shouldBe` 10
      Matchsticks.charsOfCode "\"\\x27\"" `shouldBe` 6

  describe "count characters in memory" $ do
    it "counts number of characters in memory for a single line" $ do
      Matchsticks.charsInMemory "\"\"" `shouldBe` 0
      Matchsticks.charsInMemory "\"abc\"" `shouldBe` 3
      Matchsticks.charsInMemory "\"aaa\\\"aaa\"" `shouldBe` 7
      Matchsticks.charsInMemory "\"\\x27\"" `shouldBe` 1
