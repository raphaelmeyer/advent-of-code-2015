module Main where

import qualified Day01.Floor as Floor

main :: IO ()
main = do
  description <- Floor.parseInput
  let lastFloor = Floor.whatFloor description
  let steps = Floor.enterBasement description

  putStrLn "# Day 01 #"
  putStrLn $ "Part I : " ++ show lastFloor
  putStrLn $ "Part II : " ++ show steps
