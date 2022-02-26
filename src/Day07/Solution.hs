{-# LANGUAGE OverloadedStrings #-}

module Day07.Solution where

import qualified Day07.Circuits as Circuits

run :: IO ()
run = do
  input <- Circuits.readInput "data/day-07.txt"
  let circuit = Circuits.parseInput input

  let a = Circuits.evaluate circuit "a"

  let circuit' = Circuits.overwrite circuit "b" a
  let a' = Circuits.evaluate circuit' "a"

  putStrLn ""
  putStrLn "# Day 07 #"
  putStrLn ""
  putStrLn $ "Part I : " ++ show a
  putStrLn $ "Part II : " ++ show a'
