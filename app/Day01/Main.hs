module Main where

import           Day01.Floor

main :: IO ()
main = do
  putStrLn "# Day 01 #"
  f <- readFile "data/day-01.txt"
  let description = head (lines f)
  let lastFloor   = whatFloor description
  putStrLn $ "Part I : " ++ show lastFloor
  let steps = enterBasement description
  putStrLn $ "Part II : " ++ show steps
