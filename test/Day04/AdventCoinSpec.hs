{-# LANGUAGE OverloadedStrings #-}

module Day04.AdventCoinSpec where

import Day04.AdventCoin
import Test.Hspec

spec :: Spec
spec = do
  describe "calculate nonce for md5 hash with 5 leading zeros" $ do
    it "should find nonce 609043 for abcdef" $ do
      pending
      findNonce 5 "abcdef" `shouldBe` 609043

    it "should find nonce 1048970 for pqrstuv" $ do
      pending
      findNonce 5 "pqrstuv" `shouldBe` 1048970
