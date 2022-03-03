{-# LANGUAGE OverloadedStrings #-}

module Day08.Solution where

import qualified Day08.Matchsticks as Matchsticks

run :: IO ()
run = do
  input <- Matchsticks.readInput "data/day-08.txt"

  let diff = Matchsticks.diffCodeMemory input
  let diff' = Matchsticks.diffEncodedCode input

  putStrLn ""
  putStrLn "# Day 08 #"
  putStrLn ""
  putStrLn $ "Part I : " ++ show diff
  putStrLn $ "Part II : " ++ show diff'
