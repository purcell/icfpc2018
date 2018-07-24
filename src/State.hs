{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

module State where

import Cmd (Cmd)
import Data.Map.Strict as Map
import Geometry (Coordinate(..), origin)
import qualified Matrix
import Matrix (Matrix)

data State = State
  { energy :: Energy
  , harmonics :: Harmonics
  , matrix :: Matrix
  , target :: Matrix
  , bots :: Map BotId Bot
  , trace :: [Cmd]
  } deriving (Show, Eq, Ord)

initialState :: Matrix -> State
initialState target =
  State
    { energy = 0
    , harmonics = Low
    , matrix = Matrix.emptyCopy target
    , target = target
    , bots = Map.fromList [(initialBotId, initialBot)]
    , trace = []
    }

initialBotId :: BotId
initialBotId = BotId 1

initialBot :: Bot
initialBot = Bot origin (BotId <$> [2 .. 20])

newtype Energy =
  Energy Integer
  deriving (Num, Show, Eq, Ord, Enum, Real, Integral)

data Harmonics
  = Low
  | High
  deriving (Show, Eq, Ord)

data Bot = Bot
  { coord :: Coordinate
  , seeds :: [BotId]
  } deriving (Show, Eq, Ord)

newtype BotId =
  BotId Int
  deriving (Eq, Ord, Show)

unfilledCount :: State -> Int
unfilledCount State {..} = Matrix.filledCount target - Matrix.filledCount matrix

filledCount :: State -> Int
filledCount State {..} = Matrix.filledCount matrix

allFilled :: State -> Bool
allFilled s = unfilledCount s == 0

checkForm :: State -> Either String State
checkForm state@State {..} = Right state
  -- groundingMalformed harmonics matrix = Left "All voxels must be grounded if harmonics are low"
  -- Each active nanobot has a different identifier.
  -- The position of each active nanobot is distinct and is Void in the matrix.
  -- The seeds of each active nanobot are disjoint.
  -- The seeds of each active nanobot does not include the identifier of any active nanobot.
