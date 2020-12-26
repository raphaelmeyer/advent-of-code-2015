module Day03.Delivery where

import Data.Functor ((<&>))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text

type Pos = (Int, Int)

data Path = Path Pos (Set Pos)

parseInput :: IO Text
parseInput = readFile "data/day-03.txt" <&> Text.strip . Text.pack

housesVisited :: Text -> Int
housesVisited directions = Set.size visited
  where
    (Path _ visited) = visitHouses directions

housesVisitedWithRobo :: Text -> Int
housesVisitedWithRobo directions = Set.size (Set.union visitedBySanta visitedByRobo)
  where
    (pathSanta, pathRobo) = splitPath directions
    (Path _ visitedBySanta) = visitHouses pathSanta
    (Path _ visitedByRobo) = visitHouses pathRobo

visitHouses :: Text -> Path
visitHouses = Text.foldl moveAndDeliver start
  where
    start = Path (0, 0) (Set.singleton (0, 0))

moveAndDeliver :: Path -> Char -> Path
moveAndDeliver (Path position visited) direction = Path newPosition (Set.insert newPosition visited)
  where
    move (x, y) = case direction of
      '>' -> (x + 1, y)
      '<' -> (x -1, y)
      '^' -> (x, y - 1)
      'v' -> (x, y + 1)
      _ -> (x, y)
    newPosition = move position

splitPath :: Text -> (Text, Text)
splitPath directions = (santa, robo)
  where
    pairs = Text.chunksOf 2 directions
    santa = Text.pack (map Text.head pairs)
    robo = Text.concat (map Text.tail pairs)
