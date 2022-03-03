module Day08.Matchsticks where

import Data.Functor ((<&>))
import qualified Data.Text as Text

data Expect = Any | Escaped | TwoDigits | Digit deriving (Eq, Show)

readInput :: String -> IO [Text.Text]
readInput filename = readFile filename <&> Text.lines . Text.pack

diffCodeMemory :: [Text.Text] -> Int
diffCodeMemory input = (sum . map charsOfCode) input - (sum . map charsInMemory) input

diffEncodedCode :: [Text.Text] -> Int
diffEncodedCode input = (sum . map charsEncodedCode) input - (sum . map charsOfCode) input

charsOfCode :: Text.Text -> Int
charsOfCode = Text.length

charsInMemory :: Text.Text -> Int
charsInMemory = fst . Text.foldl fold' (0, Any) . Text.tail . Text.init
  where
    fold' (count, Digit) _ = (count + 1, Any)
    fold' (count, TwoDigits) _ = (count, Digit)
    fold' (count, Escaped) 'x' = (count, TwoDigits)
    fold' (count, Escaped) '\"' = (count + 1, Any)
    fold' (count, Escaped) '\\' = (count + 1, Any)
    fold' (count, Any) '\\' = (count, Escaped)
    fold' (count, Any) _ = (count + 1, Any)
    fold' _ _ = undefined

charsEncodedCode :: Text.Text -> Int
charsEncodedCode = Text.foldl fold' 2
  where
    fold' count '\\' = count + 2
    fold' count '\"' = count + 2
    fold' count _ = count + 1
