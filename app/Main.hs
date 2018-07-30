module Main where

import Model
import Solver
import State (trace)
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
    Just (endState, energy) -> do
      print energy
      dumpTrace (trace endState) traceFile
    _ -> die "No solution found"
