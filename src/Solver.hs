{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}

module Solver
  ( solve
  ) where

import Cmd
import Control.Applicative ((<|>))
import Control.Arrow ((&&&))
import Control.Monad (when)
import Control.Monad.Reader
import Data.Foldable (for_, traverse_)
import Data.List (minimumBy, sortOn)
import qualified Data.Map as Map
import Data.Ord (comparing)
import qualified Debug.Trace
import Geometry
  ( Coordinate(..)
  , LLD(..)
  , NCD(..)
  , VectorDiff(..)
  , diffCoords
  , manhattanDistance
  , nearCoordinateDiffs
  , origin
  , translateBy
  )
import qualified Matrix
import Matrix (Matrix)
import Model
import State
import Trace (unsafeDumpTrace)
import Update

debug :: String -> a -> a
debug = const id -- or Debug.Trace.trace

type Solver = Matrix -> Build ()

solve :: Matrix -> Maybe (State, Energy)
solve m = (id &&& energy) <$> runBuild (bottomUpSolver m) (initialState m)

bottomUpSolver :: Solver
bottomUpSolver target = do
  traverse_ fillNext fillSequence
  flipH Low
  moveTo origin
  halt
  where
    fillNext x = do
      alreadyFilled <- reader (flip Matrix.isFilled x . matrix)
      unless alreadyFilled $ do
        moveTo (justAbove x)
        fillAllBelow
    fillSequence = sortOn (coordIndex target) (Matrix.toList target)

flipH :: Harmonics -> Build ()
flipH h = do
  h' <- reader harmonics
  when (h /= h') $
    debug "Flipping harmonics" $ timestep [(BotId 1, FlipHarmonics)]

-- Assumes we're above all filled pixels
moveTo :: Coordinate -> Build ()
moveTo !c = do
  [(botId, bot)] <- reader (Map.toList . bots)
  let botPos = coord bot
  when (botPos /= c) $ do
    let possibleLLD lld
          | cy botPos < cy c = dy lld > 0
          | cx botPos /= cx c || cz botPos /= cz c = dy lld == 0
          | otherwise = dy lld /= 0
    let move = bestSMove botPos possibleLLD
    debug ("Moving towards " ++ show c ++ " from " ++ show botPos) $
      timestep [(botId, move)]
    moveTo c
  where
    bestSMove a f =
      head $
      fst <$>
      sortOn
        snd
        [ (move, manhattanDistance (diffCoords (translateBy vec a) c))
        | move@(SMove (LLD vec)) <- smoves
        , f vec
        ]

fillAllBelow :: Build ()
fillAllBelow =
  debug "Fill below current location" $ do
    steps <- reader fillCmds
    for_ steps $ \cmd -> timestep [cmd] <|> (flipH High >> timestep [cmd])
  where
    fillCmds s =
      [ (botId, Fill ncd)
      | ncd@(NCD vec) <- nearCoordinateDiffs
      , dy vec == -1
      , (botId, Bot {..}) <- Map.toList (bots s)
      , let fillLoc = translateBy vec coord
      , Matrix.isValidCoord (matrix s) fillLoc
      , Matrix.isFilled (target s) fillLoc
      , not (Matrix.isFilled (matrix s) fillLoc)
      ]

justAbove :: Coordinate -> Coordinate
justAbove c = c {cy = cy c + 1}

nextPositionToFill :: (Coordinate -> Int) -> State -> Maybe Coordinate
nextPositionToFill fillOrder s = earliestUnfilled
  where
    earliestUnfilled =
      if null unfilled
        then Nothing
        else Just $
             minimumBy (comparing fillOrder) $
             takeWhile ((<= cy botPos) . cy) unfilled
    botPos = coord $ head $ Map.elems (bots s)
    unfilled = Matrix.toList $ target s `Matrix.difference` matrix s

halt :: Build ()
halt = do
  firstBotId <- reader (head . Map.keys . bots)
  debug "Halt " $ timestep [(firstBotId, Halt)]

coordIndex :: Matrix -> Coordinate -> Int
coordIndex m Coordinate {..} =
  cy * (mx * mx) +
  mx *
  (if even cy
     then cx
     else mx - cx - 1) +
  if even cx
    then cz
    else mx - cz - 1
  where
    mx = Matrix.resolution m

coordFromIndex :: Matrix -> Int -> Coordinate
coordFromIndex m i = Coordinate x y z
  where
    mx = Matrix.resolution m
    y = i `div` (mx * mx)
    x =
      if even y
        then k
        else mx - k - 1
    k = (i - (y * mx * mx)) `div` mx
    z =
      if even x
        then j
        else mx - j - 1
    j = i - (y * mx * mx) - (x * mx)

distBetween :: Coordinate -> Coordinate -> Int
distBetween c c' = manhattanDistance (diffCoords c c')
