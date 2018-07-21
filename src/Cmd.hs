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
  | SMove LLD
  | LMove SLD SLD
  | Fission NCD SeedAmount
  | FusionP NCD
  | FusionS NCD
  | Fill NCD

newtype SeedAmount = SeedAmount Int

-- LLD: Long Linear Coordinate Difference
-- SLD: Short Linear Coordinate Difference
-- NCD: Near Coordinate Difference

data VectorDiff = VectorDiff
  { dx :: Int
  , dy :: Int
  , dz :: Int
  } deriving (Eq, Ord, Show)

newtype LLD = LLD VectorDiff
newtype SLD = SLD VectorDiff
newtype NCD = NCD VectorDiff
