{-# LANGUAGE RecordWildCards #-}

module Geometry where

data Coordinate = Coordinate
  { cx :: !Int
  , cy :: !Int
  , cz :: !Int
  } deriving (Eq, Ord, Show)

origin :: Coordinate
origin = Coordinate 0 0 0

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

mkLLD :: VectorDiff -> Maybe LLD
mkLLD v@VectorDiff {..}
  | differsOnSingleAxis v = Just $ LLD v
  | otherwise = Nothing

newtype SLD =
  SLD VectorDiff
  deriving (Show, Eq, Ord)

newtype NCD =
  NCD VectorDiff
  deriving (Show, Eq, Ord)

manhattanDistance :: VectorDiff -> Int
manhattanDistance VectorDiff {..} = abs dx + abs dy + abs dz

chessboardLength :: VectorDiff -> Int
chessboardLength VectorDiff {..} = maximum [abs dx, abs dy, abs dz]

diffCoords :: Coordinate -> Coordinate -> VectorDiff
diffCoords c1 c2 = VectorDiff (cx c2 - cx c1) (cy c2 - cy c1) (cz c2 - cz c1)

translateBy :: VectorDiff -> Coordinate -> Coordinate
translateBy VectorDiff {..} Coordinate {..} =
  Coordinate {cx = cx + dx, cy = cy + dy, cz = cz + dz}

linearRegion :: VectorDiff -> Coordinate -> Coordinate -> [Coordinate]
linearRegion VectorDiff {..} c c' =
  [ Coordinate x y z
  | x <- [(cx smaller) .. (cx bigger)]
  , y <- [(cy smaller) .. (cy bigger)]
  , z <- [(cz smaller) .. (cz bigger)]
  ]
  where
    (smaller, bigger) =
      if dx + dy + dz > 0
        then (c, c')
        else (c', c)

differsOnSingleAxis :: VectorDiff -> Bool
differsOnSingleAxis v =
  manhattanDistance v > 0 && manhattanDistance v == chessboardLength v
