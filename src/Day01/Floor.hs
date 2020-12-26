module Day01.Floor
  ( enterBasement,
    parseInput,
    whatFloor,
  )
where

import Data.Functor ((<&>))
import Data.Text (Text)
import qualified Data.Text as Text

parseInput :: IO Text
parseInput = readFile "data/day-01.txt" <&> Text.strip . Text.pack

whatFloor :: Text -> Int
whatFloor = sum . map upDown . Text.unpack

enterBasement :: Text -> Maybe Int
enterBasement description = walk 0 1 (map upDown . Text.unpack $ description)

walk :: Int -> Int -> [Int] -> Maybe Int
walk _ _ [] = Nothing
walk f n (x : xs)
  | f + x < 0 = Just n
  | otherwise = walk (f + x) (n + 1) xs

upDown :: Char -> Int
upDown x = case x of
  '(' -> 1
  ')' -> -1
  _ -> error "invalid value"
