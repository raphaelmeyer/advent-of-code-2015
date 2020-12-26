{-# LANGUAGE OverloadedStrings #-}

module Day03.DeliverySpec where

import qualified Data.Set as Set
import Day03.Delivery
import Test.Hspec

spec :: Spec
spec = do
  describe "counting visited houses" $ do
    it "> delivers presents to 2 houses" $ do
      housesVisited ">" `shouldBe` 2

    it "^>v< delivers presents to 4 houses" $ do
      housesVisited "^>v<" `shouldBe` 4

  describe "move and deliver" $ do
    it "> move right" $ do
      let start = Path (0, 0) (Set.singleton (0, 0))
      let (Path pos _) = moveAndDeliver start '>'
      pos `shouldBe` (1, 0)

    it "< move left" $ do
      let start = Path (0, 0) (Set.singleton (0, 0))
      let (Path pos _) = moveAndDeliver start '<'
      pos `shouldBe` (-1, 0)

    it "^ move up" $ do
      let start = Path (0, 0) (Set.singleton (0, 0))
      let (Path pos _) = moveAndDeliver start '^'
      pos `shouldBe` (0, -1)

    it "v move down" $ do
      let start = Path (0, 0) (Set.singleton (0, 0))
      let (Path pos _) = moveAndDeliver start 'v'
      pos `shouldBe` (0, 1)

    it "add visited house to list" $ do
      let start = Path (2, 3) (Set.fromList [(0, 0), (0, 1), (1, 1), (2, 1), (2, 2), (2, 3)])
      let (Path _ visited) = moveAndDeliver start '<'
      Set.size visited `shouldBe` 7
      Set.member (1, 3) visited `shouldBe` True

    it "do not add an already visited house to the list" $ do
      let start = Path (0, 1) (Set.fromList [(0, 0), (0, 1)])
      let (Path pos visited) = moveAndDeliver start '^'
      pos `shouldBe` (0, 0)
      Set.size visited `shouldBe` 2

  describe "visit houses" $ do
    it "should visit all houses on the path" $ do
      let (Path pos visited) = visitHouses ">>^<vv"
      pos `shouldBe` (1, 1)
      Set.size visited `shouldBe` 6

  describe "counting visited houses with robo santa" $ do
    it "^v delivers presents to 3 houses" $ do
      housesVisitedWithRobo "^v" `shouldBe` 3

    it "^>v< now delivers presents to 3 houses" $ do
      housesVisitedWithRobo "^>v<" `shouldBe` 3

    it "^v^v^v^v^v now delivers presents to 11 houses" $ do
      housesVisitedWithRobo "^v^v^v^v^v" `shouldBe` 11

  describe "split path for santa and robo" $ do
    it "splits ^>v< into ^v and ><" $ do
      splitPath "^>v<" `shouldBe` ("^v", "><")

    it "splits <v> into <> and v" $ do
      splitPath "<v>" `shouldBe` ("<>", "v")
