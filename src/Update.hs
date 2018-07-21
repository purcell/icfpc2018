{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Update where

import Cmd (Cmd(..), LLD(..), NCD(..), SLD(..), VectorDiff(..))
import Control.Applicative
import Control.Monad (guard)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Model (Coordinate(..), Matrix(..), fillVoxel, isFilled, isValidCoord)
import State (BotId, Energy(..), Harmonics(..), State(..), coord)
import qualified State

performCommand :: (Monad m, Alternative m) => (BotId, Cmd) -> State -> m State
performCommand (botId, cmd) state@State {..} =
  (\s -> s {trace = trace ++ [cmd]}) <$> newState
  where
    bot = bots Map.! botId
    regionIsClear = not . any (isFilled matrix)
    newState =
      case cmd of
        Halt -> do
          guard (onlyOneBot && botAtOrigin && isComplete)
          pure $ state {bots = Map.empty}
          where isComplete = matrix == target
                onlyOneBot = length bots == 1
                botAtOrigin = coord bot == State.origin
        Wait -> pure state
        FlipHarmonics -> pure state {harmonics = flipHarmonics harmonics}
        SMove (LLD vector) -> do
          guard $ isValidCoord matrix newBotCoord
          guard $ regionIsClear regionPassedThrough
          pure
            state
              { bots = Map.insert botId movedBot bots
              , energy = energy + Energy (fromIntegral energyToMoveBot)
              }
          where movedBot = bot {coord = newBotCoord}
                newBotCoord = translateBy vector $ coord bot
                regionPassedThrough = region (coord bot) newBotCoord
                energyToMoveBot = manhattanDistance vector * 2
        LMove (SLD vector1) (SLD vector2) ->
          pure
            state
              { bots = Map.insert botId movedBot bots
              , energy = energy + Energy (fromIntegral energyToMoveBot)
              }
          where movedBot = bot {coord = newBotCoord}
                newBotCoord =
                  (translateBy vector1 . translateBy vector2) $ coord bot
                energyToMoveBot =
                  2 *
                  (manhattanDistance vector1 + 2 + manhattanDistance vector2)
        Fission _ncd _seedAmount -> undefined
        FusionP _ncd -> undefined
        FusionS _ncd -> undefined
        Fill (NCD vector)
          -- TODO: ensure the filled voxel is grounded
         -> do
          guard $ isFilled target coordToFill
          pure
            state
              { matrix = fillVoxel matrix coordToFill
              , energy = energy + energyToFillVoxel
              }
          where coordToFill = translateBy vector $ coord bot
                energyToFillVoxel =
                  if isFilled matrix coordToFill
                    then 6
                    else 12

region :: Coordinate -> Coordinate -> [Coordinate]
region c1 c2 =
  [ Coordinate x y z
  | x <- [(cx c1) .. (cx c2)]
  , y <- [(cy c1) .. (cy c2)]
  , z <- [(cz c1) .. (cz c2)]
  ]

translateBy :: VectorDiff -> Coordinate -> Coordinate
translateBy VectorDiff {..} Coordinate {..} =
  Coordinate {cx = cx + dx, cy = cy + dy, cz = cz + dz}

manhattanDistance :: VectorDiff -> Int
manhattanDistance VectorDiff {..} = abs dx + abs dy + abs dz

chessboardLength :: VectorDiff -> Int
chessboardLength VectorDiff {..} = maximum [abs dx, abs dy, abs dz]

flipHarmonics :: Harmonics -> Harmonics
flipHarmonics High = Low
flipHarmonics Low = High
