{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}

module Matrix
  ( Matrix
  , makeMatrix
  , resolution
  , fillVoxel
  , isFilled
  , filter
  , null
  , isGrounded
  , allGrounded
  , filledNeighbours
  , touchesNeighbourOrGround
  , isValidCoord
  , coordRange
  , showSlice
  , emptyCopy
  , difference
  , filledCount
  , toList
  ) where

import Data.Foldable (foldl')
import qualified Data.IntSet as IS
import Data.IntSet (IntSet)
import qualified Data.Set as S
import Prelude hiding (filter, null)
import qualified Prelude

import Geometry (Coordinate(..))

data Matrix = Matrix
  { resolution :: !Int
  , filledVoxels :: !IntSet
  } deriving (Eq, Ord, Show)

linearise :: Int -> Coordinate -> Int
linearise !r Coordinate {..} = cy * r * r + cx * r + cz

unlinearise :: Int -> Int -> Coordinate
unlinearise !r !i = Coordinate x y z
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

union :: Matrix -> Matrix -> Matrix
union m1 m2 = m1 {filledVoxels = filledVoxels m1 `IS.union` filledVoxels m2}

filter :: (Coordinate -> Bool) -> Matrix -> Matrix
filter f m@Matrix {..} =
  m {filledVoxels = IS.filter (f . unlinearise resolution) filledVoxels}

filledNeighbours :: Matrix -> Matrix -> Matrix
filledNeighbours full slice = expandedSlice `difference` slice
  where
    expandedSlice = foldl' fillVoxel (emptyCopy slice) filledNearby
    filledNearby =
      [ neighbour
      | c <- toList slice
      , neighbour <- nonDiagonalNeighbours full c
      , isFilled full neighbour
      ]

allGrounded :: Matrix -> Bool
allGrounded m@Matrix {..} = m == expandAllFrom (emptyCopy m) lowest
  where
    expandAllFrom seen cur
      | null cur = seen
      | otherwise =
        expandAllFrom
          (seen `union` cur)
          (filledNeighbours m cur `difference` seen)
    lowest = Matrix.filter ((0 ==) . cy) m

null :: Matrix -> Bool
null = IS.null . filledVoxels

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
        (Prelude.filter (`S.notMember` seen) (nonDiagonalNeighbours m c))

touchesNeighbourOrGround :: Matrix -> Coordinate -> Bool
touchesNeighbourOrGround m c =
  cy c == 0 || any (isFilled m) (nonDiagonalNeighbours m c)

nonDiagonalNeighbours :: Matrix -> Coordinate -> [Coordinate]
nonDiagonalNeighbours m Coordinate {..} =
  Prelude.filter
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
