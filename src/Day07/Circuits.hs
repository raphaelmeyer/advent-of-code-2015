{-# LANGUAGE OverloadedStrings #-}

module Day07.Circuits where

import Control.Applicative (Alternative ((<|>)))
import Data.Bits ((.&.), (.|.))
import qualified Data.Bits as Bits
import Data.Functor ((<&>))
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Data.Void (Void)
import Data.Word (Word16)
import qualified Text.Megaparsec as MP
import Text.Megaparsec.Char as C (letterChar, space, string)
import qualified Text.Megaparsec.Char.Lexer as L

type Wire = Text.Text

data Gate
  = Value Word16
  | Connect Wire
  | And Wire Wire
  | OneAnd Wire
  | Or Wire Wire
  | Not Wire
  | LeftShift Wire Int
  | RightShift Wire Int
  deriving (Eq, Show)

type Block = (Wire, Gate)

type Blocks = [Block]

type Circuit = Map.Map Wire Gate

type Memo = Map.Map Wire Word16

readInput :: String -> IO [Text.Text]
readInput filename = readFile filename <&> Text.lines . Text.pack

parseInput :: [Text.Text] -> Circuit
parseInput = Map.fromList . map parseLine

evaluate :: Circuit -> Wire -> Word16
evaluate circuit wire = evaluate' Map.empty circuit wire Map.! wire

evaluate' :: Memo -> Circuit -> Wire -> Memo
evaluate' me circuit wire = case me Map.!? wire of
  Just value -> Map.insert wire value me
  Nothing -> case circuit Map.!? wire of
    Just (Value v) -> Map.insert wire v me
    Just (Connect a) -> unary id a
    Just (And a b) -> binary (.&.) a b
    Just (OneAnd a) -> unary (1 .&.) a
    Just (Or a b) -> binary (.|.) a b
    Just (LeftShift a v) -> unary (`Bits.shiftL` v) a
    Just (RightShift a v) -> unary (`Bits.shiftR` v) a
    Just (Not a) -> unary Bits.complement a
    _ -> error $ show wire ++ " -> " ++ show (circuit Map.!? wire)
    where
      unary f a = Map.insert wire (f (me' Map.! a)) me'
        where
          me' = evaluate' me circuit a
      binary f a b = Map.insert wire (f (me'' Map.! a) (me'' Map.! b)) me''
        where
          me'' = evaluate' me' circuit b
          me' = evaluate' me circuit a

overwrite :: Circuit -> Wire -> Word16 -> Circuit
overwrite circuit wire value = Map.update (const (Just (Value value))) wire circuit

-- parse input

type Parser = MP.Parsec Void Text.Text

parseLine :: Text.Text -> Block
parseLine input = case MP.runParser grammar "" input of
  Left _ -> error $ Text.unpack input
  Right gate -> gate

grammar :: Parser Block
grammar =
  (\a b -> (b, a))
    <$> ( MP.try (Not <$ token "NOT" <*> identifier)
            <|> MP.try (OneAnd <$ token "1 AND" <*> identifier)
            <|> MP.try (And <$> identifier <* token "AND" <*> identifier)
            <|> MP.try (Or <$> identifier <* token "OR" <*> identifier)
            <|> MP.try (LeftShift <$> identifier <* token "LSHIFT" <*> number)
            <|> MP.try (RightShift <$> identifier <* token "RSHIFT" <*> number)
            <|> MP.try (Value <$> number)
            <|> (Connect <$> identifier)
        )
      <* token "->" <*> identifier

number :: Integral a => Parser a
number = L.lexeme C.space L.decimal

token :: Text.Text -> Parser Text.Text
token s = L.lexeme C.space (C.string s)

identifier :: Parser Text.Text
identifier = Text.pack <$> L.lexeme C.space (MP.many C.letterChar)
