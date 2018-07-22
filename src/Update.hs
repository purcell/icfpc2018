{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Update where

import Cmd (Cmd(..))
import Control.Applicative
import Control.Monad (guard)
import qualified Data.Map.Strict as Map
import Geometry
import Model (fillVoxel, isFilled, isGrounded, isValidCoord)
import State (BotId, Energy(..), Harmonics(..), State(..), coord)

performCommand :: (Monad m, Alternative m) => (BotId, Cmd) -> State -> m State
performCommand (botId, cmd) state@State {..} =
  (\s -> s {trace = trace ++ [cmd]}) <$> newState
  where
    bot = bots Map.! botId
    regionIsClear = not . any (isFilled matrix)
    newState =
      case cmd of
        Halt -> do
          guard $ length bots == 1
          guard $ coord bot == origin
          guard $ matrix == target
          guard $ harmonics == Low
          pure $ state {bots = Map.empty}
        Wait -> pure state
        FlipHarmonics -> pure state {harmonics = flipHarmonics harmonics}
        SMove (LLD vector) -> do
          guard $ isValidCoord matrix newBotCoord
          guard $ regionIsClear regionPassedThrough
          guard $ differsOnSingleAxis vector
          pure
            state
              { bots = Map.insert botId movedBot bots
              , energy = energy + Energy (fromIntegral energyToMoveBot)
              }
          where movedBot = bot {coord = newBotCoord}
                newBotCoord = translateBy vector $ coord bot
                regionPassedThrough =
                  linearRegion vector (coord bot) newBotCoord
                energyToMoveBot = manhattanDistance vector * 2
        LMove (SLD vector1) (SLD vector2) -> do
          guard $ isValidCoord matrix coord''
          guard $ regionIsClear regionPassedThrough
          pure
            state
              { bots = Map.insert botId movedBot bots
              , energy = energy + Energy (fromIntegral energyToMoveBot)
              }
          where movedBot = bot {coord = coord''}
                coord' = translateBy vector1 $ coord bot
                coord'' = translateBy vector2 coord'
                regionPassedThrough =
                  linearRegion vector1 (coord bot) coord' ++
                  linearRegion vector2 coord' coord''
                energyToMoveBot =
                  2 *
                  (manhattanDistance vector1 + 2 + manhattanDistance vector2)
        Fission _ncd _seedAmount -> undefined
        FusionP _ncd -> undefined
        FusionS _ncd -> undefined
        Fill (NCD vector) -> do
          guard $ isFilled target coordToFill
          guard $ isGrounded updatedMatrix coordToFill
          pure
            state {matrix = updatedMatrix, energy = energy + energyToFillVoxel}
          where coordToFill = translateBy vector $ coord bot
                (updatedMatrix, energyToFillVoxel) =
                  if isFilled matrix coordToFill
                    then (matrix, 6)
                    else (fillVoxel matrix coordToFill, 12)

flipHarmonics :: Harmonics -> Harmonics
flipHarmonics High = Low
flipHarmonics Low = High
