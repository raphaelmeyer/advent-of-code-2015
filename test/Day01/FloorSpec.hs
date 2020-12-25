module Day01.FloorSpec where

import           Test.Hspec
import           Day01.Floor

spec :: Spec
spec = do
  describe "confusing directions" $ do
    it "(()) and ()() both result in floor 0" $ do
      whatFloor "(())" `shouldBe` (0 :: Int)
      whatFloor "()()" `shouldBe` (0 :: Int)


-- (()) and ()() both result in floor 0.
-- ((( and (()(()( both result in floor 3.
-- ))((((( also results in floor 3.
-- ()) and ))( both result in floor -1 (the first basement level).
-- ))) and )())()) both result in floor -3.

