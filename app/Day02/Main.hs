module Main where

import qualified Day02.Wrapping as Wrapping

main :: IO ()
main = do
  input <- Wrapping.parseInput
  let paper = Wrapping.totalPaper input
  let ribbon = Wrapping.totalRibbon input

  putStrLn "# Day 02 #"
  putStrLn $ "Part I : " ++ show paper
  putStrLn $ "Part II : " ++ show ribbon
