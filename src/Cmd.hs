module Cmd
  ( Cmd(..)
  , SeedAmount(..)
  , mkLLD
  , lmoves
  , smoves
  ) where

import Data.Maybe (catMaybes)
import Geometry (LLD, NCD, SLD(..), linearVectorDiffs, mkLLD)

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

smoves :: [Cmd]
smoves = SMove <$> catMaybes (mkLLD <$> linearVectorDiffs 15)

lmoves :: [Cmd]
lmoves =
  [ LMove (SLD sld1) (SLD sld2)
  | sld1 <- linearVectorDiffs 5
  , sld2 <- linearVectorDiffs 5
  ]
