{-# LANGUAGE OverloadedStrings #-}

module Day05.NiceStringSpec where

import Day05.NiceString
import Test.Hspec

spec :: Spec
spec = do
  describe "old rules for nice strings" $ do
    it "ugknbfddgicrmopn is nice because of three vowels and double letter and none of the disallowed substrings" $ do
      check Old "ugknbfddgicrmopn" `shouldBe` Nice

    it "ugknbfddgicrmopn is nice because it has at least three vowels and a double letter" $ do
      check Old "aaa" `shouldBe` Nice

    it "jchzalrnumimnmhp is naughty because it has no double letter" $ do
      check Old "jchzalrnumimnmhp" `shouldBe` Naughty

    it "haegwjzuvuyypxyu is naughty because it contains the string xy" $ do
      check Old "haegwjzuvuyypxyu" `shouldBe` Naughty

    it "dvszwmarrgswjxmb is naughty because it contains only one vowel" $ do
      check Old "dvszwmarrgswjxmb" `shouldBe` Naughty

  describe "new rules for nice strings" $ do
    it "qjhvhtzxzqqjkmpb is nice because has pair (qj) and letter that repeats (zxz)" $ do
      check New "qjhvhtzxzqqjkmpb" `shouldBe` Nice

    it "xxyxx is nice because it has pair and repeat" $ do
      check New "xxyxx" `shouldBe` Nice

    it "uurcxstgmygtbstg is naughty because it has no repeat" $ do
      check New "uurcxstgmygtbstg" `shouldBe` Naughty

    it "ieodomkazucvgmuy is naughty because it has no pair" $ do
      check New "ieodomkazucvgmuy" `shouldBe` Naughty

    it "aaa is naughty because pair overlaps" $ do
      check New "aaa" `shouldBe` Naughty
