{-# LANGUAGE RecordWildCards #-}

module Solver where

import Astar
import Cmd
import qualified Data.Map as Map
import Data.Maybe (listToMaybe)
import qualified Data.Set as Set
import Model
import State
import Trace (unsafeDumpTrace)
import Update

solve :: Matrix -> Maybe (State, Int)
solve m =
  listToMaybe $
  filter (elem Halt . trace . fst) $ astar (traceSome . nexts) (initialState m)

traceSome :: [(State, Int, Int)] -> [(State, Int, Int)]
traceSome things = [maybeTrace s `seq` t | t@(s, _, _) <- things]
  where
    maybeTrace state = ()
      -- if length (trace state) == 3
      --   then unsafeDumpTrace (trace state)
      --   else ()

nexts :: State -> [(State, Int, Int)]
nexts state =
  [ (nextState, cost, distanceFromCompletion nextState)
  | nextState <- movesFromState state
  , let cost = fromIntegral (energy nextState - energy state)
  ]

movesFromState :: State -> [State]
movesFromState s =
  [s' | botCmd <- possibleCommands s, Just s' <- [performCommand botCmd s]]

possibleCommands :: State -> [(BotId, Cmd)]
possibleCommands s =
  [ (botId, cmd)
  | (botId, bot) <- Map.toList (bots s)
  , cmd <- commandsForBot s bot
  ]

commandsForBot :: State -> Bot -> [Cmd]
commandsForBot _state _bot = [Halt] ++ smoves ++ lmoves ++ fills
  where
    deltas = [-15 .. (-1)] ++ [1 .. 15]
    fills = Fill <$> nearCoordinateDiffs
    smoves =
      SMove . LLD <$>
      ([VectorDiff d 0 0 | d <- deltas] ++
       [VectorDiff 0 d 0 | d <- deltas] ++ [VectorDiff 0 0 d | d <- deltas])
    lmoves = []

nearCoordinateDiffs :: [NCD]
nearCoordinateDiffs =
  NCD <$>
  [ diff
  | dx' <- [-1 .. 1]
  , dy' <- [-1 .. 1]
  , dz' <- [-1 .. 1]
  , let diff = VectorDiff dx' dy' dz'
  , isNCD diff
  ]
    -- TODO: should be a smart constructor somewhere
  where
    isNCD d = mlen > 0 && mlen <= 2 && chessboardLength d == 1
      where
        mlen = manhattanDistance d

distanceFromCompletion :: State -> Int
distanceFromCompletion s =
  remainingBots + harmonicSettingDistance +
  distancesFromNearestNeededFillLocations
  where
    unfilledVoxels =
      matrixFilledVoxels (target s) `Set.difference`
      matrixFilledVoxels (matrix s)
    distancesFromNearestNeededFillLocations =
      sum
        [ manhattanDistance (diffCoords coord unfilled)
        | Bot {..} <- Map.elems (bots s)
        , unfilled <- Set.toList unfilledVoxels
        ]
    remainingBots = length (bots s)
    harmonicSettingDistance =
      case harmonics s of
        Low -> 0
        High -> 1
