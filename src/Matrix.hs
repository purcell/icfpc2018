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

import qualified Data.IntSet as IS
import Data.IntSet (IntSet)
import qualified Data.Set as S

import Geometry (Coordinate(..))

data Matrix = Matrix
  { resolution :: !Int
  , filledVoxels :: IntSet
  } deriving (Eq, Ord, Show)

linearise :: Int -> Coordinate -> Int
linearise r Coordinate {..} = cy * r * r + cx * r + cz

unlinearise :: Int -> Int -> Coordinate
unlinearise r i = Coordinate x y z
  where
    y = i `div` (r * r)
    (x, z) = (i - y * (r * r)) `divMod` r

makeMatrix :: Int -> [Coordinate] -> Matrix
makeMatrix n cs = Matrix n (IS.fromList (linearise n <$> cs))

emptyCopy :: Matrix -> Matrix
emptyCopy m = m {filledVoxels = IS.empty}

difference :: Matrix -> Matrix -> Matrix
difference a b
  | resolution a /= resolution b = error "mismatched dimensions"
difference a b =
  Matrix (resolution a) (filledVoxels a `IS.difference` filledVoxels b)

toList :: Matrix -> [Coordinate]
toList m = unlinearise (resolution m) <$> IS.toList (filledVoxels m)

filledCount :: Matrix -> Int
filledCount m = IS.size (filledVoxels m)

isFilled :: Matrix -> Coordinate -> Bool
isFilled Matrix {..} c = linearise resolution c `IS.member` filledVoxels

fillVoxel :: Matrix -> Coordinate -> Matrix
fillVoxel m@Matrix {..} c =
  m {filledVoxels = IS.insert (linearise resolution c) filledVoxels}

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
    inRange i = i >= 0 && i < resolution m

coordRange :: Matrix -> [Int]
coordRange Matrix {..} = [0 .. resolution - 1]

showSlice :: Matrix -> Int -> String
showSlice m y = unlines (row <$> reverse (coordRange m))
  where
    row z =
      [ if isFilled m (Coordinate x y z)
        then 'X'
        else '.'
      | x <- coordRange m
      ]
