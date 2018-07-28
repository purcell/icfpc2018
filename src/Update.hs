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
newtype Execute a = Execute
  { runExecute :: MaybeT (ST.State State) a
  } deriving ( Functor
             , Alternative
             , Applicative
             , Monad
             , MonadPlus
             , MonadState State
             )

newtype Build a = Build
  { unBuild :: Execute a
  } deriving (Functor, Alternative, Applicative, Monad, MonadPlus)

instance (MonadReader State) Build where
  ask = Build get
  local f =
    Build . Execute . MaybeT . ST.withState f . runMaybeT . runExecute . unBuild

runBuild :: Build a -> State -> Maybe State
runBuild (Build b) s =
  case ST.runState (runMaybeT (runExecute b)) s of
    (Just _, s') -> Just s'
    _ -> Nothing

-- Obsolete interface
performCommand :: (BotId, Cmd) -> State -> Maybe State
performCommand (botId, cmd) = runBuild (timestep [(botId, cmd)])

------------------------------------------------------------------------------
-- Applying commands
------------------------------------------------------------------------------
timestep :: [(BotId, Cmd)] -> Build ()
timestep instructions =
  Build $ do
    tsCost <- Energy <$> gets timestepCost
    for_ instructions $ \(botId, cmd) -> do
      bot <- getBot botId
      markVolatile [coord bot]
      apply (botId, bot) cmd
      modify (\s@State {..} -> s {trace = trace ++ [cmd]})
    addCost tsCost
  where
    timestepCost State {..} =
      (20 * fromIntegral (length bots)) +
      ((fromIntegral (Matrix.resolution matrix) ^ (3 :: Integer)) *
       case harmonics of
         High -> 30
         Low -> 3)

markVolatile :: [Coordinate] -> Execute ()
markVolatile _ = return ()

apply :: (BotId, Bot) -> Cmd -> Execute ()
apply (_, bot) Halt = do
  s@State {..} <- get
  guard $ length bots == 1
  guard $ coord bot == origin
  guard $ allFilled s
  guard $ harmonics == Low
  modify $ \s -> s {bots = Map.empty}
apply _ Wait = return ()
apply _ FlipHarmonics = do
  s@State {..} <- get
  guard $ harmonics == Low || Matrix.allGrounded matrix
  modify $ \s -> s {harmonics = flipHarmonics harmonics}
  where
    flipHarmonics High = Low
    flipHarmonics Low = High
apply (botId, bot) (SMove (LLD vector))
      -- TODO: check <= 15
 = do
  State {..} <- get
  guard $ differsOnSingleAxis vector
  let newBotCoord = translateBy vector $ coord bot
  guard $ Matrix.isValidCoord matrix newBotCoord
  let region = linearRegion vector (coord bot) newBotCoord
  traverseRegion region
  markVolatile region
  setBot botId bot {coord = newBotCoord}
  addCost $ Energy (fromIntegral (manhattanDistance vector * 2))
apply (botId, bot) (LMove (SLD vector1) (SLD vector2))
      -- TODO: check <= 5
 = do
  State {..} <- get
  let coord' = translateBy vector1 $ coord bot
  guard $ Matrix.isValidCoord matrix coord'
  let coord'' = translateBy vector2 coord'
  guard $ Matrix.isValidCoord matrix coord''
  let region =
        linearRegion vector1 (coord bot) coord' ++
        linearRegion vector2 coord' coord''
  traverseRegion region
  markVolatile region
  setBot botId bot {coord = coord''}
  addCost $
    Energy
      (fromIntegral
         (2 * (manhattanDistance vector1 + 2 + manhattanDistance vector2)))
apply _ (Fission _ncd _seedAmount) = undefined
apply _ (FusionP _ncd) = undefined
apply _ (FusionS _ncd) = undefined
apply (_, bot) (Fill (NCD vector)) = do
  State {..} <- get
  let coordToFill = translateBy vector $ coord bot
  guard $ Matrix.isFilled target coordToFill
  markVolatile [coordToFill]
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
getBot :: BotId -> Execute Bot
getBot botId = gets bots >>= (maybe mzero return . Map.lookup botId)

setBot :: BotId -> Bot -> Execute ()
setBot botId bot = modify $ \s -> s {bots = Map.insert botId bot (bots s)}

addCost :: Energy -> Execute ()
addCost i = modify (\s@State {..} -> s {energy = energy + i})

regionIsClear :: [Coordinate] -> Execute Bool
regionIsClear coords = noneFilled <$> gets matrix
  where
    noneFilled m = not (any (Matrix.isFilled m) coords)

traverseRegion :: [Coordinate] -> Execute ()
traverseRegion r = guard =<< regionIsClear r
