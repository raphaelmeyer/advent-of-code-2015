module Day01.Solution where

import qualified Day01.Floor as Floor

run :: IO ()
run = do
  description <- Floor.parseInput
  let lastFloor = Floor.whatFloor description
  let steps = Floor.enterBasement description

  putStrLn ""
  putStrLn "# Day 01 #"
  putStrLn ""
  putStrLn $ "Part I : " ++ show lastFloor
  putStrLn $ "Part II : " ++ show steps
