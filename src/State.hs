{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module State where

import           Cmd             (Cmd)
import           Data.Map.Strict as Map
import           Data.Set        (Set)
import qualified Data.Set        as Set
import           Model           (Coordinate (..), Matrix (..))

data State =
  State { energy       :: Energy
        , harmonics    :: Harmonics
        , filledVoxels :: Set Coordinate
        , matrix       :: Matrix
        , bots         :: Map BotId Bot
        , trace        :: [Cmd]
        } deriving (Eq, Ord)

initialState :: Matrix -> State
initialState m =
  State { energy = 0
        , harmonics = Low
        , filledVoxels = Set.empty
        , matrix = m { matrixFilledVoxels = Set.empty }
        , bots = Map.fromList [ (BotId 1, Bot origin (BotId <$> [2..20]))]
        , trace = [] }

origin :: Coordinate
origin = Coordinate 0 0 0

newtype Energy = Energy Int deriving (Num, Eq, Ord, Enum, Real, Integral)

data Harmonics = Low | High deriving (Eq, Ord)

data Bot =
  Bot { coord :: Coordinate
      , seeds :: [BotId]
      } deriving (Eq, Ord)

newtype BotId = BotId Int deriving (Eq, Ord)

