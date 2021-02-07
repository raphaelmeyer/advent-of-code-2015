{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Day04.AdventCoin as AdventCoin

main :: IO ()
main = do
  let nonce5 = AdventCoin.findNonce 5 "iwrupvqb"
  let nonce6 = AdventCoin.findNonce 6 "iwrupvqb"

  putStrLn "# Day 04 #"
  putStrLn $ "Part I : " ++ show nonce5
  putStrLn $ "Part II : " ++ show nonce6
