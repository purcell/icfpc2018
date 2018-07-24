{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}

module Solver
  ( solve
  ) where

import Cmd
import Control.Applicative ((<|>), empty)
import Control.Arrow ((&&&))
import Data.List (minimumBy, sortOn)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, listToMaybe)
import Data.Ord (comparing)
import qualified Data.Set as Set
import qualified Debug.Trace
import Geometry
  ( Coordinate(..)
  , LLD(..)
  , NCD(..)
  , SLD(..)
  , VectorDiff(..)
  , diffCoords
  , linearVectorDiffs
  , manhattanDistance
  , mkLLD
  , nearCoordinateDiffs
  , origin
  , translateBy
  )
import qualified Matrix
import Matrix (Matrix)
import State
import Trace (unsafeDumpTrace)
import Update

debug :: String -> a -> a
debug = const id -- or Debug.Trace.trace

solve :: Matrix -> Maybe (State, Int)
solve m = (id &&& (fromIntegral . energy)) <$> go (initialState m)
  where
    go :: State -> Maybe State
    go !s
      | allFilled s = flipH Low s >>= (`moveTo` origin) >>= halt
    go !s = (fillAround s <|> moveNext s) >>= go
    moveNext :: State -> Maybe State
    moveNext s =
      debug "Looking for next position" $
      nextPositionToFill (coordIndex m) s >>= moveTo s . justAbove

flipH :: Harmonics -> State -> Maybe State
flipH h s
  | harmonics s == h = Just s
flipH _ s =
  debug "Flipping harmonics" $ performCommand (BotId 1, FlipHarmonics) s

-- Assumes we're above all filled pixels
moveTo :: State -> Coordinate -> Maybe State
moveTo s !c = debug ("Find best move to " ++ show c) $ maybeMove s
  where
    botId = head (Map.keys (bots s))
    botPos st = coord (bots st Map.! botId)
    -- Shortest smove from c to a, with LLD satisfying f
    bestSMove a f =
      listToMaybe $
      fst <$>
      sortOn
        snd
        [ (move, manhattanDistance (diffCoords (translateBy vec a) c))
        | move@(SMove (LLD vec)) <- smoves
        , f vec
        ]
    moveThenCont s' move = performCommand (botId, move) s' >>= maybeMove
    maybeMove s'
      | botPos s' == c = pure s'
      | cy (botPos s') < cy c =
        bestSMove (botPos s') (\lld -> dy lld > 0) >>= moveThenCont s'
      | cx (botPos s') /= cx c || cz (botPos s') /= cz c =
        bestSMove (botPos s') (\lld -> dy lld == 0) >>= moveThenCont s'
      | otherwise =
        bestSMove (botPos s') (\lld -> dy lld /= 0) >>= moveThenCont s'

fillAround :: State -> Maybe State
fillAround s =
  debug "Try to fill around current location" $
  if null fillCmds
    then empty
    else listToMaybe fillCmds >>= \c ->
           performCommand c s <|> (flipH High s >>= performCommand c)
  where
    fillCmds =
      [ (botId, Fill ncd)
      | ncd@(NCD vec) <- nearCoordinateDiffs
      , (botId, Bot {..}) <- Map.toList (bots s)
      , let fillLoc = translateBy vec coord
      , Matrix.isFilled (target s) fillLoc
      , not (Matrix.isFilled (matrix s) fillLoc)
      , cy fillLoc < cy coord
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

halt :: State -> Maybe State
halt s = debug "Halt " $ performCommand (head (Map.keys (bots s)), Halt) s

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

smoves :: [Cmd]
smoves = SMove <$> catMaybes (mkLLD <$> linearVectorDiffs 15)

lmoves :: [Cmd]
lmoves =
  [ LMove (SLD sld1) (SLD sld2)
  | sld1 <- linearVectorDiffs 5
  , sld2 <- linearVectorDiffs 5
  ]
