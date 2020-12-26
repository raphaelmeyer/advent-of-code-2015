module Main where

import Day01.Floor

main :: IO ()
main = do
  f <- readFile "data/day-01.txt"
  let description = head (lines f)
  let lastFloor = whatFloor description
  let steps = enterBasement description

  putStrLn "# Day 01 #"
  putStrLn $ "Part I : " ++ show lastFloor
  putStrLn $ "Part II : " ++ show steps
