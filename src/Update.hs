{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}

module Update where

import           Cmd             (Cmd (..), LLD (..), NCD (..))
import qualified Cmd
import qualified Data.Map        (Map)
import           Data.Map.Strict as Map
import qualified Data.Set        as Set
import           Debug.Trace     as Debug
import           Model           (Coordinate (..))
import           State           (Bot, BotId, Energy, Harmonics (..),
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
        SMove lld              ->
          state { bots = Map.insert botId movedBot bots
                , energy = energy + energyToMoveBot
                }
          where movedBot = bot { coord = coordFromLLD }
                coordFromLLD = addLLD lld $ coord bot
                energyToMoveBot = magnitudeOfLLD * 2
                magnitudeOfLLD = x lld + y lld + z lld
        LMove sld1 sld2        -> undefined
        Fission ncd seedAmount -> undefined
        FusionP ncd            -> undefined
        FusionS ncd            -> undefined
        Fill ncd               ->
          state { filledVoxels = Set.insert coordFromNCD filledVoxels
                , energy = energy + energyToFillVoxel
                }
          where coordFromNCD = addNCD ncd $ coord bot
                energyToFillVoxel =
                  if Set.member coordFromNCD filledVoxels then
                    6
                  else
                    12

{-
Let c′ = c + lld.
It is an error if c′ is not a valid coordinate with respect to the resolution of the matrix.
It is an error if any coordinate in the region [c,c′] is Full in the matrix.
The volatile coordinates of this command are all coordinates in the region [c,c′].
The effect of this command is:
bot.pos := c′
(The nanobot’s position is updated and there is an energy cost proportional the Manhattan length of the move.)
-}


addLLD :: LLD -> Coordinate -> Coordinate
addLLD (LLD (dx, dy, dz)) Coordinate{..} =
  Coordinate { cx = cx + dx, cy = cy + dy, cz = cz + dz }

addNCD :: NCD -> Coordinate -> Coordinate
addNCD (NCD (dx, dy, dz)) Coordinate{..} =
  Coordinate { cx = cx + dx, cy = cy + dy, cz = cz + dz }

flipHarmonics :: Harmonics -> Harmonics
flipHarmonics High = Low
flipHarmonics Low  = High
