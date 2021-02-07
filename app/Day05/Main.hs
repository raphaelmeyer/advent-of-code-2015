module Main where

import qualified Day05.NiceString as NiceString

main :: IO ()
main = do
  input <- NiceString.parseInput

  putStrLn "# Day 05 #"
  putStrLn $ "Part I : " ++ show (NiceString.countNice NiceString.Old input)
  putStrLn $ "Part II : " ++ show (NiceString.countNice NiceString.New input)
