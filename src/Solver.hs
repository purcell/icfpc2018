{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}

module Solver
  ( solve
  ) where

import Cmd
import Control.Applicative ((<|>), empty)
import Control.Arrow ((&&&))
import Control.Monad (foldM)
import Data.List (minimumBy, sortOn)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, listToMaybe)
import Data.Ord (comparing)
import qualified Data.Set as Set
import qualified Debug.Trace as Debug
import Geometry
  ( Coordinate(..)
  , LLD(..)
  , NCD(..)
  , SLD(..)
  , VectorDiff(..)
  , chessboardLength
  , diffCoords
  , linearVectorDiffs
  , manhattanDistance
  , mkLLD
  , nearCoordinateDiffs
  , origin
  , surroundingCoords
  , translateBy
  )
import Model
import State
import Trace (unsafeDumpTrace)
import Update

solve :: Matrix -> Maybe (State, Int)
solve m = (id &&& (fromIntegral . energy)) <$> go (initialState m)
  where
    go :: State -> Maybe State
    go !s
      | allFilled s = flipH Low s >>= (`moveTo` origin) >>= halt
    go !s = (fillAround s <|> moveNext s) >>= go
    moveNext :: State -> Maybe State
    moveNext s =
      Debug.trace "Looking for next position" $
      bestPositionToFill (coordIndex m) s >>= moveTo s

flipH :: Harmonics -> State -> Maybe State
flipH h s
  | harmonics s == h = Just s
flipH _ s =
  Debug.trace "Flipping harmonics" $ performCommand (BotId 1, FlipHarmonics) s

moveTo :: State -> Coordinate -> Maybe State
moveTo s !c = Debug.trace ("Find best move to " ++ show c) $ maybeMove s
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
  Debug.trace "Try to fill around current location" $
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
      , isFilled (target s) fillLoc
      , not (isFilled (matrix s) fillLoc)
      , cy fillLoc < cy coord
      ]

bestPositionToFill :: (Coordinate -> Int) -> State -> Maybe Coordinate
bestPositionToFill fillOrder s = (\c -> c {cy = cy c + 1}) <$> earliestUnfilled
  where
    earliestUnfilled =
      if null unfilled
        then Nothing
        else Just $ minimumBy (comparing fillOrder) unfilled
    unfilled = Set.toList $ unfilledVoxels s

halt :: State -> Maybe State
halt s = Debug.trace "Halt " $ performCommand (head (Map.keys (bots s)), Halt) s

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
    mx = matrixResolution m

coordFromIndex :: Matrix -> Int -> Coordinate
coordFromIndex m i = Coordinate x y z
  where
    mx = matrixResolution m
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
