{-# LANGUAGE RecordWildCards #-}

module Matrix
  ( Matrix
  , makeMatrix
  , resolution
  , fillVoxel
  , isFilled
  , isGrounded
  , isValidCoord
  , coordRange
  , showSlice
  , emptyCopy
  , difference
  , filledCount
  , toList
  ) where

import qualified Data.Set as S

import Geometry (Coordinate(..))

data Matrix = Matrix
  { matrixResolution :: !Int
  , matrixFilledVoxels :: S.Set Coordinate
  } deriving (Eq, Ord, Show)

resolution :: Matrix -> Int
resolution = matrixResolution

makeMatrix :: Int -> S.Set Coordinate -> Matrix
makeMatrix n cs = Matrix n cs

emptyCopy :: Matrix -> Matrix
emptyCopy m = m {matrixFilledVoxels = S.empty}

difference :: Matrix -> Matrix -> Matrix
difference a b
  | matrixResolution a /= matrixResolution b = error "mismatched dimensions"
difference a b =
  Matrix
    (matrixResolution a)
    (matrixFilledVoxels a `S.difference` matrixFilledVoxels b)

toList :: Matrix -> [Coordinate]
toList = S.toList . matrixFilledVoxels

filledCount :: Matrix -> Int
filledCount m = S.size (matrixFilledVoxels m)

isFilled :: Matrix -> Coordinate -> Bool
isFilled Matrix {..} c = c `S.member` matrixFilledVoxels

fillVoxel :: Matrix -> Coordinate -> Matrix
fillVoxel m@Matrix {..} c =
  m {matrixFilledVoxels = S.insert c matrixFilledVoxels}

isGrounded :: Matrix -> Coordinate -> Bool
isGrounded = go S.empty
  where
    go :: S.Set Coordinate -> Matrix -> Coordinate -> Bool
    go _ m c
      | not (isFilled m c) = False
    go _ _ c
      | cy c == 0 = True
    go seen m c =
      any
        (go (S.insert c seen) m)
        (filter (`S.notMember` seen) (nonDiagonalNeighbours m c))

nonDiagonalNeighbours :: Matrix -> Coordinate -> [Coordinate]
nonDiagonalNeighbours m Coordinate {..} =
  filter
    (isValidCoord m)
    [ Coordinate (cx + dx) (cy + dy) (cz + dz)
    | (dx, dy, dz) <-
        [(-1, 0, 0), (0, -1, 0), (0, 0, -1), (1, 0, 0), (0, 1, 0), (0, 0, 1)]
    ]

isValidCoord :: Matrix -> Coordinate -> Bool
isValidCoord m Coordinate {..} = inRange cx && inRange cy && inRange cz
  where
    inRange i = i >= 0 && i < matrixResolution m

coordRange :: Matrix -> [Int]
coordRange Matrix {..} = [0 .. matrixResolution - 1]

showSlice :: Matrix -> Int -> String
showSlice m y = unlines (row <$> reverse (coordRange m))
  where
    row z =
      [ if isFilled m (Coordinate x y z)
        then 'X'
        else '.'
      | x <- coordRange m
      ]
