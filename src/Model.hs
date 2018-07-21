{-# LANGUAGE RecordWildCards #-}

module Model where

import Data.Binary.Get
import Data.Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Vector as V
import Data.Word

data VoxelState
  = Void
  | Full
  deriving (Show, Eq)

data Coordinate = Coordinate
  { cx :: Int
  , cy :: Int
  , cz :: Int
  } deriving (Eq, Ord, Show)

data Matrix = Matrix
  { matrixResolution :: Int
  , matrixVoxelState :: Coordinate -> VoxelState
  }

isGrounded :: Matrix -> Coordinate -> Bool
isGrounded m c
  | Void == matrixVoxelState m c = False
isGrounded _ c
  | cy c == 0 = True
isGrounded m c = any (isGrounded m) (nonDiagonalNeighbours m c)

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
      [voxelChar (matrixVoxelState m (Coordinate x y z)) | x <- coordRange m]
    voxelChar Void = '.'
    voxelChar Full = 'X'

modelFromFile :: FilePath -> IO Matrix
modelFromFile path = runGet getMatrix <$> BSL.readFile path

getMatrix :: Get Matrix
getMatrix = do
  res <- fromIntegral <$> getWord8
  let bytesToRead = ceiling (fromIntegral (res * res * res) / 8 :: Rational)
  bytes <- V.fromList . BS.unpack <$> getByteString bytesToRead
  return (Matrix res (voxelStateAt bytes res))

voxelStateAt :: V.Vector Word8 -> Int -> Coordinate -> VoxelState
voxelStateAt bytes res Coordinate {..} =
  if testBit (bytes V.! byteIdx) bitIdx
    then Full
    else Void
  where
    offset = cx * res * res + cy * res + cz
    (byteIdx, bitIdx) = offset `divMod` 8
