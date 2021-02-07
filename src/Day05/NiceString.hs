module Day05.NiceString where

import Data.Functor ((<&>))
import qualified Data.Text as Text

data Niceness = Nice | Naughty deriving (Show, Eq)

data Rules = New | Old

countNice :: Rules -> [Text.Text] -> Int
countNice ruleset = foldl addNice 0
  where
    addNice acc string = case check ruleset string of
      Nice -> acc + 1
      Naughty -> acc

check :: Rules -> Text.Text -> Niceness
check ruleset string = if all ($ string) rules then Nice else Naughty
  where
    rules = case ruleset of
      Old -> [three_vowels, double_letter, no_forbidden]
      New -> [repeat_letter, double_pair]

parseInput :: IO [Text.Text]
parseInput = readFile "data/day-05.txt" <&> Text.lines . Text.pack

three_vowels :: Text.Text -> Bool
three_vowels = (>= 3) . Text.length . Text.filter (`elem` "aiueo")

double_letter :: Text.Text -> Bool
double_letter string = any (uncurry (==)) (Text.zip <*> Text.tail $ string)

no_forbidden :: Text.Text -> Bool
no_forbidden string = all ((== 0) . (\r -> Text.count (Text.pack r) string)) ["ab", "cd", "pq", "xy"]

double_pair :: Text.Text -> Bool
double_pair string
  | Text.length string < 4 = False
  | pair `Text.isInfixOf` (Text.drop 2 string) = True
  | otherwise = double_pair (Text.tail string)
  where
    pair = Text.take 2 string

repeat_letter :: Text.Text -> Bool
repeat_letter string
  | Text.length string < 3 = False
  | a == b = True
  | otherwise = repeat_letter (Text.tail string)
  where
    h = Text.take 3 string
    a = Text.head h
    b = Text.last h
