{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}

module State where

import           Cmd             (Cmd)
import           Data.Map.Strict as Map
import           Data.Set        (Set)
import qualified Data.Set        as Set
import           Model           (Coordinate (..))

data State =
  State { energy       :: Energy
        , harmonics    :: Harmonics
        , filledVoxels :: Set Coordinate
        , bots         :: Map BotId Bot
        , trace        :: [Cmd]
        } deriving (Show, Eq, Ord)

initialState :: State
initialState =
  State { energy = 0
        , harmonics = Low
        , filledVoxels = Set.empty
        , bots = Map.fromList [ (initialBotId, initialBot)]
        , trace = [] }

initialBotId :: BotId
initialBotId = BotId 1

initialBot :: Bot
initialBot =
  Bot origin (BotId <$> [2..20])

origin :: Coordinate
origin = Coordinate 0 0 0

newtype Energy = Energy Int deriving (Num, Show, Eq, Ord, Enum, Real, Integral)

data Harmonics = Low | High deriving (Show, Eq, Ord)

data Bot =
  Bot { coord :: Coordinate
      , seeds :: [BotId]
      } deriving (Show, Eq, Ord)

newtype BotId = BotId Int deriving (Eq, Ord, Show)

checkForm :: State -> Either String State
checkForm state@State{ .. } = Right state
  -- groundingMalformed harmonics matrix = Left "All voxels must be grounded if harmonics are low"
  -- Each active nanobot has a different identifier.
  -- The position of each active nanobot is distinct and is Void in the matrix.
  -- The seeds of each active nanobot are disjoint.
  -- The seeds of each active nanobot does not include the identifier of any active nanobot.
