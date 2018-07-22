{-# LANGUAGE RecordWildCards #-}

module Model where

import Data.Binary.Get
import Data.Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Set as S
import qualified Data.Vector as V
import Data.Word

data Coordinate = Coordinate
  { cx :: !Int
  , cy :: !Int
  , cz :: !Int
  } deriving (Eq, Ord, Show)

data Matrix = Matrix
  { matrixResolution :: !Int
  , matrixFilledVoxels :: S.Set Coordinate
  } deriving (Eq, Ord, Show)

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

modelFromFile :: FilePath -> IO Matrix
modelFromFile path = runGet getMatrix <$> BSL.readFile path

getMatrix :: Get Matrix
getMatrix = do
  res <- fromIntegral <$> getWord8
  let bytesToRead = ceiling (fromIntegral (res * res * res) / 8 :: Rational)
  bytes <- V.fromList . BS.unpack <$> getByteString bytesToRead
  let filledCoords =
        S.fromList
          [ c
          | x <- [1 .. res - 2]
          , y <- [0 .. res - 2]
          , z <- [1 .. res - 2]
          , let c = Coordinate x y z
          , voxelFilledAt bytes res c
          ]
  return (Matrix res filledCoords)

voxelFilledAt :: V.Vector Word8 -> Int -> Coordinate -> Bool
voxelFilledAt bytes res Coordinate {..} = testBit (bytes V.! byteIdx) bitIdx
  where
    offset = cx * res * res + cy * res + cz
    (byteIdx, bitIdx) = offset `divMod` 8
