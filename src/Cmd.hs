module Cmd
  ( Cmd(..)
  , SeedAmount(..)
  , mkLLD
  ) where

import Geometry (LLD, NCD, SLD, mkLLD)

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
