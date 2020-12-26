{-# LANGUAGE OverloadedStrings #-}

module Day02.Wrapping
  ( Box (..),
    parseInput,
    parsePresent,
    requiredPaper,
    requiredRibbon,
    totalPaper,
    totalRibbon,
  )
where

import Data.Functor ((<&>))
import Data.List (sort)
import Data.Maybe (mapMaybe)
import qualified Data.Text as Text
import qualified Data.Text.Read as Read (decimal)

data Box = Box Int Int Int deriving (Show, Eq)

parseInput :: IO [Box]
parseInput = readFile "data/day-02.txt" <&> mapMaybe parsePresent . Text.lines . Text.pack

parsePresent :: Text.Text -> Maybe Box
parsePresent input = case dimensions of
  [Right (l, _), Right (w, _), Right (h, _)] -> Just (Box l w h)
  _ -> Nothing
  where
    dimensions = map Read.decimal (Text.split (== 'x') input)

totalPaper :: [Box] -> Int
totalPaper = total requiredPaper

totalRibbon :: [Box] -> Int
totalRibbon = total requiredRibbon

requiredPaper :: Box -> Int
requiredPaper (Box l w h) = sum (map (2 *) sides) + smallest
  where
    sides = [l * w, w * h, l * h]
    smallest = minimum sides

requiredRibbon :: Box -> Int
requiredRibbon (Box l w h) = volume + bow
  where
    volume = 2 * (sum . take 2 . sort $ [l, h, w])
    bow = l * w * h

total :: (Box -> Int) -> [Box] -> Int
total f = sum . map f
