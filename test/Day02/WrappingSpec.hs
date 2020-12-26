{-# LANGUAGE OverloadedStrings #-}

module Day02.WrappingSpec where

import Day02.Wrapping
import Test.Hspec

spec :: Spec
spec = do
  describe "required wrapping paper" $ do
    it "present with dimensions 2x3x4 requires 58 square feet" $
      requiredPaper (Box 2 3 4) `shouldBe` 58

    it "present with dimensions 1x1x10 requires 43 square feet" $
      requiredPaper (Box 1 1 10) `shouldBe` 43

    it "present with dimensions 3x1x2 requires 24 square feet" $
      requiredPaper (Box 3 1 2) `shouldBe` 24

  describe "total paper" $ do
    it "should calculate total required paper" $ do
      let presents = [Box 4 2 3, Box 3 2 1, Box 1 10 1]
      totalPaper presents `shouldBe` 125

  describe "parse present" $ do
    it "parses a presents dimension" $
      parsePresent "17x23x42" `shouldBe` Just (Box 17 23 42)

    it "returns nothing if input not valid" $ do
      parsePresent "" `shouldBe` Nothing
      parsePresent "1x2" `shouldBe` Nothing
      parsePresent "miximaxi" `shouldBe` Nothing

  describe "required ribbon" $ do
    it "present with dimensions 2x3x4 requires 34 feet of ribbon" $
      requiredRibbon (Box 2 3 4) `shouldBe` 34

    it "present with dimensions 1x1x10 requires 14 feet of ribbon" $
      requiredRibbon (Box 10 1 1) `shouldBe` 14

  describe "total ribbon" $ do
    it "should calculate total required ribbon" $ do
      let presents = [Box 1 10 1, Box 4 2 3]
      totalRibbon presents `shouldBe` 48
