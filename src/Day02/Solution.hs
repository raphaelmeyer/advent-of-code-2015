module Day02.Solution where

import qualified Day02.Wrapping as Wrapping

run :: IO ()
run = do
  input <- Wrapping.parseInput
  let paper = Wrapping.totalPaper input
  let ribbon = Wrapping.totalRibbon input

  putStrLn ""
  putStrLn "# Day 02 #"
  putStrLn ""
  putStrLn $ "Part I : " ++ show paper
  putStrLn $ "Part II : " ++ show ribbon
