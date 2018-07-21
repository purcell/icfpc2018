module Cmd
  ( Cmd(..)
  , SeedAmount(..)
  , LLD(..)
  , SLD(..)
  , NCD(..)
  ) where

data Cmd
  = Halt
  | Wait
  | FlipHarmonics
  | SMove LLD
  | LMove SLD
          SLD
  | Fission NCD
            SeedAmount
  | FusionP NCD
  | FusionS NCD
  | Fill NCD

newtype SeedAmount =
  SeedAmount Int

-- LLD: Long Linear Coordinate Difference
-- SLD: Short Linear Coordinate Difference
-- NCD: Near Coordinate Difference
newtype LLD =
  LLD (Int, Int, Int)

newtype SLD =
  SLD (Int, Int, Int)

newtype NCD =
  NCD (Int, Int, Int)
