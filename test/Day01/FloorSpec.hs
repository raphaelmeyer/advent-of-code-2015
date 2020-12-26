{-# LANGUAGE OverloadedStrings #-}

module Day01.FloorSpec where

import Day01.Floor
import Test.Hspec

spec :: Spec
spec = do
  describe "confusing directions" $ do
    it "(()) and ()() both result in floor 0" $ do
      whatFloor "(())" `shouldBe` (0 :: Int)
      whatFloor "()()" `shouldBe` (0 :: Int)

    it "((( and (()(()( both result in floor 3" $ do
      whatFloor "(((" `shouldBe` (3 :: Int)
      whatFloor "(()(()(" `shouldBe` (3 :: Int)

    it "))((((( also results in floor 3" $ do
      whatFloor "))(((((" `shouldBe` (3 :: Int)

    it "()) and ))( both result in floor -1" $ do
      whatFloor "())" `shouldBe` (-1 :: Int)
      whatFloor "))(" `shouldBe` (-1 :: Int)

    it "))) and )())()) both result in floor -3" $ do
      whatFloor ")))" `shouldBe` (-3 :: Int)
      whatFloor ")())())" `shouldBe` (-3 :: Int)

  describe "enter the basement" $ do
    it ") causes him to enter the basement at character position 1" $ do
      enterBasement ")" `shouldBe` Just (1 :: Int)

    it "()()) causes him to enter the basement at character position 5" $ do
      enterBasement "()())" `shouldBe` Just (5 :: Int)

    it "()(()())( never enter the basement" $ do
      enterBasement "()(()())(" `shouldBe` Nothing
