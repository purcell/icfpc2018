{-# LANGUAGE RecordWildCards #-}

module Update where

import           Cmd   (Cmd (..))
import qualified Cmd

import           State (Bot, BotId, Energy, Harmonics (..), State (..))
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
    Fill ncd               -> undefined

flipHarmonics :: Harmonics -> Harmonics
flipHarmonics High = Low
flipHarmonics Low  = High
