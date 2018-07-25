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
import Data.Foldable (for_)
import Data.List (minimumBy, sortOn)
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import Data.Ord (comparing)
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
solve m = (id &&& (fromIntegral . energy)) <$> runBuild go (initialState m)
  where
    fillOrder = coordIndex m
    go :: Build ()
    go =
      debug "Looking for next" $
      reader (nextPositionToFill fillOrder) >>= \case
        Just p -> do
          moveTo (justAbove p)
          fillAllBelow
          go
        Nothing -> do
          flipH Low
          moveTo origin
          halt

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
  debug "Try to fill below current location" $ do
    steps <- reader fillCmds
    for_ steps $ \cmd -> timestep [cmd] <|> (flipH High >> timestep [cmd])
  where
    fillCmds s =
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

smoves :: [Cmd]
smoves = SMove <$> catMaybes (mkLLD <$> linearVectorDiffs 15)

lmoves :: [Cmd]
lmoves =
  [ LMove (SLD sld1) (SLD sld2)
  | sld1 <- linearVectorDiffs 5
  , sld2 <- linearVectorDiffs 5
  ]
