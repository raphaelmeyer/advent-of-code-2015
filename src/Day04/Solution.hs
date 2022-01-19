{-# LANGUAGE OverloadedStrings #-}

module Day04.Solution where

import qualified Day04.AdventCoin as AdventCoin

run :: IO ()
run = do
  let nonce5 = AdventCoin.findNonce 5 "iwrupvqb"
  let nonce6 = AdventCoin.findNonce 6 "iwrupvqb"

  putStrLn ""
  putStrLn "# Day 04 #"
  putStrLn ""
  putStrLn $ "Part I : " ++ show nonce5
  putStrLn $ "Part II : " ++ show nonce6
