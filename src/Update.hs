{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Update
  ( performCommand
  , timestep
  , runBuild
  , Build
  ) where

import Cmd (Cmd(..))
import Control.Applicative
import Control.Monad (MonadPlus, guard, mzero)
import Control.Monad.Reader
import Control.Monad.State.Class
import qualified Control.Monad.State.Strict as ST
import Control.Monad.Trans.Maybe
import Data.Foldable (for_)
import qualified Data.Map.Strict as Map
import Geometry
import qualified Matrix
import State
  ( Bot
  , BotId
  , Energy(..)
  , Harmonics(..)
  , State(..)
  , allFilled
  , coord
  )

------------------------------------------------------------------------------
-- Monadic plumbing
------------------------------------------------------------------------------
newtype Build a = Build
  { unBuild :: MaybeT (ST.State State) a
  } deriving ( Functor
             , Alternative
             , Applicative
             , Monad
             , MonadPlus
             , MonadState State
             )

instance (MonadReader State) Build where
  ask = get
  local f = Build . MaybeT . ST.withState f . runMaybeT . unBuild

runBuild :: Build a -> State -> Maybe State
runBuild b s =
  case ST.runState (runMaybeT (unBuild b)) s of
    (Just _, s') -> Just s'
    _ -> Nothing

performCommand :: (BotId, Cmd) -> State -> Maybe State
performCommand (botId, cmd) = runBuild (timestep [(botId, cmd)])

------------------------------------------------------------------------------
-- Applying commands
------------------------------------------------------------------------------
timestep :: [(BotId, Cmd)] -> Build ()
timestep instructions = do
  tsCost <- Energy <$> gets timestepCost
  for_ instructions $ \(botId, cmd) -> do
    _ <- getBot botId
    apply botId cmd
    modify (\s@State {..} -> s {trace = trace ++ [cmd]})
  addCost tsCost
  where
    timestepCost State {..} =
      (20 * fromIntegral (length bots)) +
      ((fromIntegral (Matrix.resolution matrix) ^ (3 :: Integer)) *
       case harmonics of
         High -> 30
         Low -> 3)

apply :: BotId -> Cmd -> Build ()
apply botId Halt = do
  s@State {..} <- get
  guard $ length bots == 1
  bot <- getBot botId
  guard $ coord bot == origin
  guard $ allFilled s
  guard $ harmonics == Low
  modify $ \s -> s {bots = Map.empty}
apply _ Wait = return ()
apply _ FlipHarmonics
      -- TODO: assert all filled voxels are grounded if turning to Low
 = modify $ \s -> s {harmonics = flipHarmonics (harmonics s)}
  where
    flipHarmonics High = Low
    flipHarmonics Low = High
apply botId (SMove (LLD vector))
      -- TODO: check <= 15
 = do
  State {..} <- get
  bot <- getBot botId
  let newBotCoord = translateBy vector $ coord bot
  guard $ Matrix.isValidCoord matrix newBotCoord
  traverseRegion (linearRegion vector (coord bot) newBotCoord)
  guard $ differsOnSingleAxis vector
  setBot botId bot {coord = newBotCoord}
  addCost $ Energy (fromIntegral (manhattanDistance vector * 2))
apply botId (LMove (SLD vector1) (SLD vector2))
      -- TODO: check <= 5
 = do
  State {..} <- get
  bot <- getBot botId
  let coord' = translateBy vector1 $ coord bot
  guard $ Matrix.isValidCoord matrix coord'
  let coord'' = translateBy vector2 coord'
  guard $ Matrix.isValidCoord matrix coord''
  traverseRegion
    (linearRegion vector1 (coord bot) coord' ++
     linearRegion vector2 coord' coord'')
  setBot botId bot {coord = coord''}
  addCost $
    Energy
      (fromIntegral
         (2 * (manhattanDistance vector1 + 2 + manhattanDistance vector2)))
apply _ (Fission _ncd _seedAmount) = undefined
apply _ (FusionP _ncd) = undefined
apply _ (FusionS _ncd) = undefined
apply botId (Fill (NCD vector)) = do
  State {..} <- get
  bot <- getBot botId
  let coordToFill = translateBy vector $ coord bot
  guard $ Matrix.isFilled target coordToFill
  -- TODO: verify we can actually reach this coord to fill it
  if Matrix.isFilled matrix coordToFill
    then addCost 6
    else do
      let updatedMatrix = Matrix.fillVoxel matrix coordToFill
      guard $
        harmonics == High ||
        Matrix.touchesNeighbourOrGround updatedMatrix coordToFill
      modify $ \s -> s {matrix = updatedMatrix}
      addCost 12

------------------------------------------------------------------------------
-- Monadic helpers
------------------------------------------------------------------------------
getBot :: BotId -> Build Bot
getBot botId = gets bots >>= (maybe mzero return . Map.lookup botId)

setBot :: BotId -> Bot -> Build ()
setBot botId bot = modify $ \s -> s {bots = Map.insert botId bot (bots s)}

addCost :: Energy -> Build ()
addCost i = modify (\s@State {..} -> s {energy = energy + i})

regionIsClear :: [Coordinate] -> Build Bool
regionIsClear coords = noneFilled <$> gets matrix
  where
    noneFilled m = not (any (Matrix.isFilled m) coords)

traverseRegion :: [Coordinate] -> Build ()
traverseRegion r = guard =<< regionIsClear r
