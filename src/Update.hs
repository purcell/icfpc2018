{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}

module Update where

import           Cmd             (Cmd (..), LLD (..), NCD (..), VectorDiff (..))
import           Data.Map.Strict as Map
import qualified Data.Set        as Set
import           Debug.Trace     as Debug
import           Model           (Coordinate (..))
import           State           (BotId, Energy (..), Harmonics (..),
                                  State (..), coord)

import qualified State

performCommand :: (BotId, Cmd) -> State -> Maybe State
performCommand (botId, cmd) state@State{energy, filledVoxels, bots} =
  case State.checkForm newState of
    Left err -> Debug.trace err Nothing
    Right s  -> Just s
  where
    bot = bots Map.! botId
    newState =
      case cmd of
        Halt                   -> undefined

        Wait                   -> state

        FlipHarmonics          -> state { harmonics = flipHarmonics $ harmonics state }

        SMove (LLD vector)     ->
          state { bots = Map.insert botId movedBot bots
                , energy = energy + Energy energyToMoveBot
                }
          where movedBot = bot { coord = newBotCoord }
                newBotCoord = translateBy vector $ coord bot
                energyToMoveBot = manhattanDistance vector * 2

        LMove sld1 sld2        -> undefined

        Fission ncd seedAmount -> undefined

        FusionP ncd            -> undefined

        FusionS ncd            -> undefined

        Fill (NCD vector)               ->
          state { filledVoxels = Set.insert coordToFill filledVoxels
                , energy = energy + energyToFillVoxel
                }
          where coordToFill = translateBy vector $ coord bot
                energyToFillVoxel =
                  if Set.member coordToFill filledVoxels then
                    6
                  else
                    12

translateBy :: VectorDiff -> Coordinate -> Coordinate
translateBy VectorDiff{..} Coordinate{..} =
  Coordinate { cx = cx + dx, cy = cy + dy, cz = cz + dz }

manhattanDistance :: VectorDiff -> Int
manhattanDistance VectorDiff{..} = abs dx + abs dy + abs dz

flipHarmonics :: Harmonics -> Harmonics
flipHarmonics High = Low
flipHarmonics Low  = High
