{-# LANGUAGE RecordWildCards #-}

module Solver where

import Astar
import Cmd
import qualified Data.Map as Map
import Data.Maybe (catMaybes, listToMaybe)
import qualified Data.Set as Set
import Geometry
  ( Coordinate(..)
  , SLD(..)
  , VectorDiff(..)
  , chessboardLength
  , diffCoords
  , linearVectorDiffs
  , mkLLD
  , nearCoordinateDiffs
  , origin
  , surroundingCoords
  )
import Model
import State
import Trace (unsafeDumpTrace)
import Update

solve :: Matrix -> Maybe (State, Int)
solve m =
  listToMaybe $
  filter (elem Halt . trace . fst) $
  astarOn stateFingerprint (traceSome . nexts m) (initialState m)

stateFingerprint :: State -> (Set.Set Coordinate, Harmonics, Matrix)
stateFingerprint State {..} =
  ((Set.fromList (coord <$> Map.elems bots)), harmonics, matrix)

traceSome :: [(State, Int, Int)] -> [(State, Int, Int)]
traceSome things = [maybeTrace s `seq` t | t@(s, _, _) <- things]
  where
    anyFills s = not (null [True | (Fill _) <- trace s])
    allFilled s = matrix s == target s
    maybeTrace state = ()
      -- if length (trace state) == 75
      --   then unsafeDumpTrace (trace state)
      --   else ()

nexts :: Matrix -> State -> [(State, Int, Int)]
nexts m state =
  [ (nextState, cost, distanceFromCompletion (voxelConnectedness m) nextState)
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
commandsForBot _state _bot = fills ++ smoves ++ lmoves ++ [Halt]
  where
    fills = Fill <$> nearCoordinateDiffs
    smoves = SMove <$> catMaybes (mkLLD <$> linearVectorDiffs 15)
    lmoves =
      [ LMove (SLD sld1) (SLD sld2)
      | sld1 <- linearVectorDiffs 5
      , sld2 <- linearVectorDiffs 5
      ]

voxelConnectedness :: Matrix -> Coordinate -> Int
voxelConnectedness m coord = Map.findWithDefault 0 coord connectedness
  where
    connectedness =
      Map.fromList
        [(c, numToBeFilledAround c) | c <- Set.elems (matrixFilledVoxels m)]
    numToBeFilledAround c = length (filter (isFilled m) (surroundingCoords c))

distanceFromCompletion :: (Coordinate -> Int) -> State -> Int
distanceFromCompletion connectedness s =
  unfilledScore + remainingBots + harmonicSettingDistance +
  distancesFromVoxelsToBeFilled +
  distanceFromOriginOnceDone
  where
    unfilledScore =
      sum
        [ (1 +
           case harmonics s of
             Low -> cy c
             _ -> 0) *
        100 *
        connectedness c
        | c <- Set.toList unfilledVoxels
        ]
    unfilledVoxels =
      matrixFilledVoxels (target s) `Set.difference`
      matrixFilledVoxels (matrix s)
    distanceFromOriginOnceDone =
      if Set.null unfilledVoxels
        then sum
               [ chessboardLength (diffCoords coord origin)
               | Bot {..} <- Map.elems (bots s)
               ]
        else 0
    distancesFromVoxelsToBeFilled =
      if Set.null unfilledVoxels
        then 0
        else minimum
               [ chessboardLength (diffCoords coord unfilled)
               | Bot {..} <- Map.elems (bots s)
               , unfilled <- Set.toList unfilledVoxels
               ]
    remainingBots = length (bots s)
    harmonicSettingDistance =
      case harmonics s of
        Low -> 0
        High -> 1
