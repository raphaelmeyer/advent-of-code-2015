{-# LANGUAGE OverloadedStrings #-}

module Day06.LightsSpec where

import qualified Day06.Lights as Lights
import Test.Hspec

spec :: Spec
spec = do
  describe "parse input" $ do
    it "parses turn on instructions" $ do
      Lights.parseLine "turn on 0,0 through 999,999"
        `shouldBe` Lights.Instruction {Lights.getRange = Lights.Range (0, 0) (999, 999), Lights.getAction = Lights.On}

    it "parses turn off instructions" $ do
      Lights.parseLine "turn off 499,499 through 500,500"
        `shouldBe` Lights.Instruction {Lights.getRange = Lights.Range (499, 499) (500, 500), Lights.getAction = Lights.Off}

    it "parses toggle instructions" $ do
      Lights.parseLine "toggle 0,0 through 999,0"
        `shouldBe` Lights.Instruction {Lights.getRange = Lights.Range (0, 0) (999, 0), Lights.getAction = Lights.Toggle}

    it "parses input and normalizes coordinates" $ do
      let input =
            Lights.parseInput
              [ "turn on 0,0 through 999,999",
                "turn off 499,499 through 500,500",
                "toggle 0,0 through 999,0"
              ]
      input
        `shouldBe` [ Lights.Instruction {Lights.getRange = Lights.Range (0, 0) (1000, 1000), Lights.getAction = Lights.On},
                     Lights.Instruction {Lights.getRange = Lights.Range (499, 499) (501, 501), Lights.getAction = Lights.Off},
                     Lights.Instruction {Lights.getRange = Lights.Range (0, 0) (1000, 1), Lights.getAction = Lights.Toggle}
                   ]

  describe "apply instructions" $ do
    it "applies instructions for on, off and toggle" $ do
      -- 100
      -- 000
      -- 011
      let input =
            Lights.parseInput
              [ "turn on 1,1 through 3,2",
                "toggle 2,1 through 3,3",
                "turn off 1,2 through 3,2"
              ]
      let setup = Lights.setup input
      Lights.count setup `shouldBe` 3

  describe "find x and y segments" $ do
    let input =
          Lights.parseInput
            [ "turn on 9,23 through 13,27",
              "toggle 7,27 through 11,29",
              "turn off 10,24 through 12,29",
              "turn on 7,21 through 10,24"
            ]
    it "finds x segments" $ do
      Lights.getXSegments input `shouldBe` [(7, 9), (9, 10), (10, 11), (11, 12), (12, 13), (13, 14)]

    it "finds y segments" $ do
      Lights.getYSegments (7, 9) input `shouldBe` [(21, 25), (25, 27), (27, 30)]
      Lights.getYSegments (10, 11) input `shouldBe` [(21, 23), (23, 24), (24, 25), (25, 27), (27, 28), (28, 30)]

  describe "evaluate setup" $ do
    let input =
          Lights.parseInput
            [ "turn on 9,23 through 13,27",
              "toggle 7,27 through 11,29",
              "turn off 10,24 through 12,29",
              "turn on 7,21 through 10,24"
            ]
    it "finds sections with shared state based on intersections" $ do
      let setup = Lights.setup input

      length setup `shouldBe` 22

      setup `shouldContain` [Lights.Section {Lights.getArea = Lights.Range (7, 21) (9, 25), Lights.getActions = [Lights.On]}]
      setup `shouldContain` [Lights.Section {Lights.getArea = Lights.Range (10, 25) (11, 27), Lights.getActions = [Lights.Off, Lights.On]}]
      setup `shouldContain` [Lights.Section {Lights.getArea = Lights.Range (10, 27) (11, 28), Lights.getActions = [Lights.Off, Lights.Toggle, Lights.On]}]

  describe "evaluate state of lights" $ do
    it "is on when the top action is On" $ do
      let actions = [Lights.On, Lights.Off, Lights.Toggle, Lights.Off]
      Lights.state actions `shouldBe` Lights.On

    it "is off when the top action is Off" $ do
      let actions = [Lights.Off, Lights.Toggle, Lights.Off, Lights.On]
      Lights.state actions `shouldBe` Lights.Off

    it "is on when toggling from off an odd number of times" $ do
      Lights.state [Lights.Toggle, Lights.Off] `shouldBe` Lights.On
      Lights.state [Lights.Toggle, Lights.Toggle, Lights.Toggle, Lights.Off] `shouldBe` Lights.On

    it "is off when toggling from off an even number of times" $ do
      Lights.state [Lights.Toggle, Lights.Toggle, Lights.Off] `shouldBe` Lights.Off

    it "is off when toggling from on an odd number of times" $ do
      Lights.state [Lights.Toggle, Lights.On] `shouldBe` Lights.Off
      Lights.state [Lights.Toggle, Lights.Toggle, Lights.Toggle, Lights.On] `shouldBe` Lights.Off

    it "is on when toggling from on an even number of times" $ do
      Lights.state [Lights.Toggle, Lights.Toggle, Lights.On] `shouldBe` Lights.On

    it "is off when no action is define" $ do
      Lights.state [] `shouldBe` Lights.Off