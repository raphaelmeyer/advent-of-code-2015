{-# LANGUAGE OverloadedStrings #-}

module Day07.CircuitsSpec where

import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Day07.Circuits as Circuits
import Test.Hspec

exampleInput :: [Text.Text]
exampleInput =
  [ "123 -> x",
    "456 -> y",
    "x AND y -> d",
    "x OR y -> e",
    "x LSHIFT 2 -> f",
    "y RSHIFT 2 -> g",
    "NOT x -> h",
    "NOT y -> i"
  ]

spec :: Spec
spec = do
  describe "parse wiring instructions" $ do
    it "parses all instructions" $ do
      let circuit = Circuits.parseInput exampleInput

      Map.size circuit `shouldBe` 8

      circuit Map.!? "x" `shouldBe` Just (Circuits.Value 123)
      circuit Map.!? "y" `shouldBe` Just (Circuits.Value 456)
      circuit Map.!? "g" `shouldBe` Just (Circuits.RightShift "y" 2)

  describe "parse a wiring instruction" $ do
    it "parses value assignment" $ do
      Circuits.parseLine "42 -> z" `shouldBe` ("z", Circuits.Value 42)

    it "parses and gate" $ do
      Circuits.parseLine "a AND b -> c" `shouldBe` ("c", Circuits.And "a" "b")
      Circuits.parseLine "1 AND t -> u" `shouldBe` ("u", Circuits.OneAnd "t")

    it "parses or gate" $ do
      Circuits.parseLine "j OR k -> l" `shouldBe` ("l", Circuits.Or "j" "k")

    it "parses left shift gate" $ do
      Circuits.parseLine "m LSHIFT 4 -> n" `shouldBe` ("n", Circuits.LeftShift "m" 4)

    it "parses right shift gate" $ do
      Circuits.parseLine "q RSHIFT 5 -> b" `shouldBe` ("b", Circuits.RightShift "q" 5)

    it "parses not gate" $ do
      Circuits.parseLine "NOT d -> f" `shouldBe` ("f", Circuits.Not "d")

  describe "evaluate signal value" $ do
    let circuit = Circuits.parseInput exampleInput

    it "evaluates value at wire" $ do
      Circuits.evaluate circuit "x" `shouldBe` 123
      Circuits.evaluate circuit "d" `shouldBe` 72
      Circuits.evaluate circuit "e" `shouldBe` 507
      Circuits.evaluate circuit "f" `shouldBe` 492
      Circuits.evaluate circuit "g" `shouldBe` 114
      Circuits.evaluate circuit "h" `shouldBe` 65412
