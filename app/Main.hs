module Main where

import qualified Data.ByteString.Lazy as BSL
import Data.Semigroup ((<>))
import Model
import Solver
import State
import System.Environment (getArgs)
import System.Exit (die)
import Trace

main :: IO ()
main = do
  args <- getArgs
  case args of
    [problemFile, traceFile] -> solveAndTrace problemFile traceFile
    _ -> die "Usage: thisprog problemfile tracefile"

solveAndTrace :: FilePath -> FilePath -> IO ()
solveAndTrace problemFile traceFile = do
  model <- modelFromFile problemFile
  case solve model of
    Just (endState, cost) -> do
      putStrLn $ "Found solution which cost " <> show cost
      BSL.writeFile traceFile (toBinaryTrace (trace endState))
    _ -> die "No solution found"
