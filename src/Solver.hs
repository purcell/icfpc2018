{-# LANGUAGE RecordWildCards #-}

module Solver
  ( solve
  , Strategy(..)
  ) where

import Astar
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
solve m = (id &&& (fromIntegral . energy)) <$> (flipH (initialState m) >>= go)
  where
    flipH :: State -> Maybe State
    flipH = performCommand (BotId 1, FlipHarmonics)
    go :: State -> Maybe State
    go s
      | allFilled s = moveTo s origin >>= flipH >>= halt
    go s = (fillAround s <|> moveNext s) >>= go
    moveNext :: State -> Maybe State
    moveNext s =
      (Debug.trace "Looking for next position" $ bestPositionToFill s) >>=
      moveTo s
    moveTo :: State -> Coordinate -> Maybe State
    moveTo s c
      -- (if c == Coordinate 6 2 6
      --    then unsafeDumpTrace (trace s)
      --    else ()) `seq`
     =
      Debug.trace ("Find best move to " ++ show c) $
      findBest (moveToStrategy c) s
    fillAround :: State -> Maybe State
    fillAround s =
      Debug.trace "Try to fill around current location" $
      if null fillCmds
        then empty
        else listToMaybe fillCmds >>= \c -> performCommand c s
      where
        fillCmds =
          [ (botId, Fill ncd)
          | ncd@(NCD vec) <- nearCoordinateDiffs
          , (botId, Bot {..}) <- Map.toList (bots s)
          , let fillLoc = translateBy vec coord
          , isFilled m fillLoc
          , not (isFilled (matrix s) fillLoc)
          , cy fillLoc < cy coord
          ]
    bestPositionToFill :: State -> Maybe Coordinate
    bestPositionToFill s = (\c -> c {cy = cy c + 1}) <$> earliestUnfilled
      where
        earliestUnfilled =
          if null unfilled
            then Nothing
            else Just $ minimumBy (comparing (coordIndex m)) unfilled
        unfilled = Set.toList $ unfilledVoxels s
    halt :: State -> Maybe State
    halt s =
      Debug.trace "Halt " $ performCommand (head (Map.keys (bots s)), Halt) s

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

moveToStrategy :: Coordinate -> Strategy
moveToStrategy target =
  Strategy
    { heuristic = leastBotDistanceCost
    , makeCommands = moves
    , isDone = (== 0) . leastBotDistance
    }
  where
    leastBotDistanceCost s =
      ceiling (fromIntegral (leastBotDistance s) / 15 :: Rational) *
      fromIntegral (timestepCost s) +
      leastBotDistance s * 2
    leastBotDistance s =
      minimum [distBetween target coord | Bot {..} <- Map.elems (bots s)]
    moves s =
      [(botId, move) | botId <- Map.keys (bots s), move <- lmoves ++ smoves]

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

------------------------------------------------------------------------------
-- Search plumbing
------------------------------------------------------------------------------
data Strategy = Strategy
  { heuristic :: State -> Int
  , makeCommands :: State -> [(BotId, Cmd)]
  , isDone :: State -> Bool
  }

findBest :: Strategy -> State -> Maybe State
findBest strategy s =
  fmap fst $
  listToMaybe $
  filter (isDone strategy . fst) $
  astarOn stateFingerprint (traceSome . nexts strategy) s

stateFingerprint :: State -> (Map.Map BotId Bot, Harmonics, Matrix)
stateFingerprint State {..} = (bots, harmonics, matrix)

traceSome :: [(State, Int, Int)] -> [(State, Int, Int)]
traceSome things = [maybeTrace s `seq` t | t@(s, _, _) <- things]
  where
    anyFills s = not (null [True | (Fill _) <- trace s])
    allFilled s = matrix s == target s
    maybeTrace state = ()
      -- if length (trace state) == 3
      --   then unsafeDumpTrace (trace state)
      --   else ()

nexts :: Strategy -> State -> [(State, Int, Int)]
nexts strategy state =
  [ (nextState, cost, heuristic strategy nextState)
  | nextState <- movesFromState strategy state
  , let cost = fromIntegral (energy nextState)
  ]

movesFromState :: Strategy -> State -> [State]
movesFromState strategy s =
  [s' | botCmd <- makeCommands strategy s, Just s' <- [performCommand botCmd s]]
