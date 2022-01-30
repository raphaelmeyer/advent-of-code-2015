module Day06.Solution where

import qualified Day06.Lights as Lights

run :: IO ()
run = do
  input <- Lights.readInput "data/day-06.txt"
  let instructions = Lights.parseInput input

  let setup = Lights.setup instructions
  let on = Lights.count setup

  let brightness = Lights.calculateBrightness setup

  putStrLn ""
  putStrLn "# Day 06 #"
  putStrLn ""
  putStrLn $ "Part I : " ++ show on
  putStrLn $ "Part II : " ++ show brightness
