module  Cmd
    (
    ) where


data Cmd =
  Halt
  | Wait
  | FlipHarmonics
  | SMove LLD
  | LMove SLD SLD
  | Fission NCD SeedAmount
  | FusionP NCD
  | FusionS NCD
  | Fill NCD

newtype SeedAmount = SeedAmount Int
-- Long Linear Coordinate Difference : LLD
-- Short Linear Coordinate Difference : SLD
-- Near Coordinate Difference : NCD
data LLD = LLD (Int, Int, Int)
data SLD = SLD (Int, Int, Int)
data NCD = NCD (Int, Int, Int)
