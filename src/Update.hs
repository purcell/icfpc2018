{-# LANGUAGE RecordWildCards #-}

module Update where

import           Cmd      (Cmd (..), NCD (..))
import qualified Cmd
import qualified Data.Set as Set
import           Model    (Coordinate (..))
import           State    (Bot, BotId, Energy, Harmonics (..), State (..),
                           coord)
import qualified State

update :: State -> State
update state@State{..} =
  let commandsToPerform = take (length bots) trace
  in
    foldr performCommand state $ zip bots commandsToPerform


performCommand :: (Bot, Cmd) -> State -> State
performCommand (bot, cmd) state =
  case cmd of
    Halt                   -> undefined
    Wait                   -> state
    FlipHarmonics          -> state { harmonics = flipHarmonics $ harmonics state }
    SMove lld              -> undefined
    LMove sld1 sld2        -> undefined
    Fission ncd seedAmount -> undefined
    FusionP ncd            -> undefined
    FusionS ncd            -> undefined
    Fill ncd               ->
      state { filledVoxels = Set.insert coordFromNCD (filledVoxels state) }
      where coordFromNCD = addNCD (coord bot) ncd

addNCD :: Coordinate -> NCD -> Coordinate
addNCD Coordinate{..} (NCD (dx, dy, dz)) =
  Coordinate { cx = cx + dx, cy = cy + dy, cz = cz + dz }

flipHarmonics :: Harmonics -> Harmonics
flipHarmonics High = Low
flipHarmonics Low  = High
