module Main where

import qualified Day03.Delivery as Delivery

main :: IO ()
main = do
  input <- Delivery.parseInput
  let visited = Delivery.housesVisited input
  let visitedWithRobo = Delivery.housesVisitedWithRobo input

  putStrLn "# Day 03 #"
  putStrLn $ "Part I : " ++ show visited
  putStrLn $ "Part II : " ++ show visitedWithRobo
