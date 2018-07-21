module Cmd
  ( Cmd(..)
  , SeedAmount(..)
  , LLD(..)
  , SLD(..)
  , NCD(..)
  , VectorDiff(..)
  ) where

data Cmd
  = Halt
  | Wait
  | FlipHarmonics
  | SMove !LLD
  | LMove !SLD
          !SLD
  | Fission !NCD
            !SeedAmount
  | FusionP !NCD
  | FusionS !NCD
  | Fill !NCD
  deriving (Show, Eq, Ord)

newtype SeedAmount =
  SeedAmount Int
  deriving (Show, Eq, Ord)

-- LLD: Long Linear Coordinate Difference
-- SLD: Short Linear Coordinate Difference
-- NCD: Near Coordinate Difference
data VectorDiff = VectorDiff
  { dx :: !Int
  , dy :: !Int
  , dz :: !Int
  } deriving (Eq, Ord, Show)

newtype LLD =
  LLD VectorDiff
  deriving (Show, Eq, Ord)

newtype SLD =
  SLD VectorDiff
  deriving (Show, Eq, Ord)

newtype NCD =
  NCD VectorDiff
  deriving (Show, Eq, Ord)
