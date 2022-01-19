module Day05.Solution where

import qualified Day05.NiceString as NiceString

run :: IO ()
run = do
  input <- NiceString.parseInput

  putStrLn ""
  putStrLn "# Day 05 #"
  putStrLn ""
  putStrLn $ "Part I : " ++ show (NiceString.countNice NiceString.Old input)
  putStrLn $ "Part II : " ++ show (NiceString.countNice NiceString.New input)
