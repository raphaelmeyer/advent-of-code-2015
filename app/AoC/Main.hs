module Main where

import Control.Applicative ((<**>))
import qualified Control.Applicative as Applicative
import qualified Data.Map as Map
import qualified Day01.Solution as Day01
import qualified Day02.Solution as Day02
import qualified Day03.Solution as Day03
import qualified Day04.Solution as Day04
import qualified Day05.Solution as Day05
import qualified Day06.Solution as Day06
import qualified Day07.Solution as Day07
import qualified Day08.Solution as Day08
import qualified Options.Applicative as Opt

data Options = Options {getDay :: Maybe Int} deriving (Show)

options :: Opt.Parser Options
options = Options <$> Applicative.optional (Opt.option Opt.auto $ Opt.long "day" <> Opt.help "run solution of a single day" <> Opt.metavar "DAY")

solutions :: Map.Map Int (IO ())
solutions =
  Map.fromList
    [ (1, Day01.run),
      (2, Day02.run),
      (3, Day03.run),
      (4, Day04.run),
      (5, Day05.run),
      (6, Day06.run),
      (7, Day07.run),
      (8, Day08.run)
    ]

main :: IO ()
main = do
  opts <- Opt.execParser (Opt.info (options <**> Opt.helper) (Opt.fullDesc <> Opt.progDesc "run advent of code 2015 solutions" <> Opt.header "Advent of Code 2015"))
  runSolutions (getDay opts)

runSolutions :: Maybe Int -> IO ()
runSolutions Nothing = sequence_ . Map.elems $ solutions
runSolutions (Just day) = runSolution $ Map.lookup day solutions
  where
    runSolution (Just solution) = solution
    runSolution Nothing = putStrLn $ "No solution for day " ++ show day
