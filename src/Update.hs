{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}

module Update where

import           Cmd             (Cmd (..), LLD (..), NCD (..), SLD (..),
                                  VectorDiff (..))
import           Data.Map.Strict as Map
import qualified Data.Set        as Set
import           Model           (Coordinate (..), Matrix (..), fillVoxel,
                                  isFilled, isValidCoord)
import           State           (BotId, Energy (..), Harmonics (..),
                                  State (..), coord)

import qualified State

performCommand :: (BotId, Cmd) -> State -> Maybe State
performCommand (botId, cmd) state@State{..} =
  (\s -> s { trace = trace ++ [cmd] }) <$> newState
  where
    bot = bots Map.! botId
    newState =
      case cmd of
        Halt ->
          if onlyOneBot && botAtOrigin && isComplete
          then Just $ state { bots = Map.empty }
          else Nothing
          where
            isComplete = matrix == target
            onlyOneBot = length bots == 1
            botAtOrigin = coord bot == State.origin

        Wait -> Just state

        FlipHarmonics -> Just state { harmonics = flipHarmonics harmonics }

        SMove (LLD vector) ->
          if isValidCoord matrix newBotCoord
          then Just state { bots = Map.insert botId movedBot bots
                          , energy = energy + Energy energyToMoveBot
                          }
          else Nothing
          where movedBot = bot { coord = newBotCoord }
                newBotCoord = translateBy vector $ coord bot
                energyToMoveBot = manhattanDistance vector * 2

        LMove (SLD vector1) (SLD vector2) ->
          Just state { bots = Map.insert botId movedBot bots
                     , energy = energy + Energy energyToMoveBot
                     }
          where movedBot = bot { coord = newBotCoord }
                newBotCoord = (translateBy vector1 . translateBy vector2) $ coord bot
                energyToMoveBot = 2 * (manhattanDistance vector1 + 2 + manhattanDistance vector2)

        Fission _ncd _seedAmount -> undefined

        FusionP _ncd -> undefined

        FusionS _ncd -> undefined

        Fill (NCD vector) ->
          Just state { matrix = fillVoxel matrix coordToFill
                , energy = energy + energyToFillVoxel
                }
          where coordToFill = translateBy vector $ coord bot
                energyToFillVoxel =
                  if isFilled matrix coordToFill then
                    6
                  else
                    12

translateBy :: VectorDiff -> Coordinate -> Coordinate
translateBy VectorDiff{..} Coordinate{..} =
  Coordinate { cx = cx + dx, cy = cy + dy, cz = cz + dz }

manhattanDistance :: VectorDiff -> Int
manhattanDistance VectorDiff{..} = abs dx + abs dy + abs dz

chessboardLength :: VectorDiff -> Int
chessboardLength VectorDiff{..} = maximum [abs dx, abs dy, abs dz]

flipHarmonics :: Harmonics -> Harmonics
flipHarmonics High = Low
flipHarmonics Low  = High
