{-# LANGUAGE OverloadedStrings #-}

module Day06.Lights where

import Control.Applicative (Alternative ((<|>)))
import Data.Functor ((<&>))
import qualified Data.List as List
import qualified Data.Text as Text
import Data.Void (Void)
import qualified Text.Megaparsec as MP
import Text.Megaparsec.Char as C (string)
import qualified Text.Megaparsec.Char.Lexer as L

type Position = (Int, Int)

data Action = On | Off | Toggle deriving (Show, Eq)

data Range = Range Position Position deriving (Show, Eq)

data Instruction = Instruction {getRange :: Range, getAction :: Action} deriving (Show, Eq)

type Instructions = [Instruction]

data Section = Section {getArea :: Range, getActions :: [Action]} deriving (Show, Eq)

type Setup = [Section]

type Segment = (Int, Int)

readInput :: String -> IO [Text.Text]
readInput filename = readFile filename <&> Text.lines . Text.pack

parseInput :: [Text.Text] -> Instructions
parseInput = map (normalize . parseLine)

normalize :: Instruction -> Instruction
normalize instruction@Instruction {getRange = Range from (x, y)} = instruction {getRange = Range from (x + 1, y + 1)}

setup :: Instructions -> Setup
setup instructions = foldr splitSections [] xSegments
  where
    xSegments = getXSegments instructions
    splitSections xSegment@(x1, x2) sections = map makeSection (getYSegments xSegment instructions) ++ sections
      where
        makeSection (y1, y2) = Section {getArea = Range (x1, y1) (x2, y2), getActions = actions}
          where
            actions = foldl contains' [] instructions
            contains' actions' Instruction {getRange = Range (xa, ya) (xb, yb), getAction = action}
              | xa <= x1 && x2 <= xb && ya <= y1 && y2 <= yb = action : actions'
              | otherwise = actions'

count :: Setup -> Int
count = foldl count' 0
  where
    count' lights Section {getArea = area, getActions = actions}
      | state actions == On = lights + rangeArea area
      | otherwise = lights
    rangeArea (Range (x1, y1) (x2, y2)) = (x2 - x1) * (y2 - y1)

getXSegments :: Instructions -> [Segment]
getXSegments instructions = zip (init xs) (tail xs)
  where
    xs = List.nub . List.sort $ foldl getXs [] instructions
    getXs xs' Instruction {getRange = Range (x1, _) (x2, _)} = x1 : x2 : xs'

getYSegments :: Segment -> Instructions -> [Segment]
getYSegments (xa, xb) instructions = if null ys then [] else zip (init ys) (tail ys)
  where
    ys = List.nub . List.sort $ foldl getYs [] instructions
    getYs ys' Instruction {getRange = Range (x1, y1) (x2, y2)}
      | x1 <= xa && xb <= x2 = y1 : y2 : ys'
      | otherwise = ys'

state :: [Action] -> Action
state (Toggle : actions) = invert . state $ actions
state (On : _) = On
state (Off : _) = Off
state _ = Off

invert :: Action -> Action
invert On = Off
invert Off = On
invert _ = undefined

-- parse input

type Parser = MP.Parsec Void Text.Text

parseLine :: Text.Text -> Instruction
parseLine input = case MP.runParser grammar "" input of
  Left _ -> undefined
  Right instruction -> instruction

grammar :: Parser Instruction
grammar = parseOn <|> parseOff <|> parseToggle

parseOn :: Parser Instruction
parseOn = do
  _ <- C.string "turn on "
  range <- parseRange
  return Instruction {getRange = range, getAction = On}

parseOff :: Parser Instruction
parseOff = do
  _ <- C.string "turn off "
  range <- parseRange
  return Instruction {getRange = range, getAction = Off}

parseToggle :: Parser Instruction
parseToggle = do
  _ <- C.string "toggle "
  range <- parseRange
  return Instruction {getRange = range, getAction = Toggle}

parseRange :: Parser Range
parseRange = Range <$> parsePosition <* C.string " through " <*> parsePosition

parsePosition :: Parser Position
parsePosition = (,) <$> integer <* C.string "," <*> integer
  where
    integer = L.signed (pure ()) L.decimal
