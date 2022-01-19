module Day03.Solution where

import qualified Day03.Delivery as Delivery

run :: IO ()
run = do
  input <- Delivery.parseInput
  let visited = Delivery.housesVisited input
  let visitedWithRobo = Delivery.housesVisitedWithRobo input

  putStrLn ""
  putStrLn "# Day 03 #"
  putStrLn ""
  putStrLn $ "Part I : " ++ show visited
  putStrLn $ "Part II : " ++ show visitedWithRobo
