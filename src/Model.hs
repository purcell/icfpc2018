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

coordRange :: Int -> [Int]
coordRange res = [0 .. res - 1]

showSlice :: Matrix -> Int -> String
showSlice m y = unlines (row <$> reverse (coordRange (matrixResolution m)))
  where
    row z =
      [ voxelChar (matrixVoxelState m (Coordinate x y z))
      | x <- coordRange (matrixResolution m)
      ]
    voxelChar Void = '.'
    voxelChar Full = 'X'

newtype Model =
  Model Matrix

modelFromFile :: FilePath -> IO Model
modelFromFile path = runGet getModel <$> BSL.readFile path

getModel :: Get Model
getModel = do
  res <- fromIntegral <$> getWord8
  let bytesToRead = ceiling (fromIntegral (res * res * res) / 8 :: Rational)
  bytes <- V.fromList . BS.unpack <$> getByteString bytesToRead
  return (Model (Matrix res (voxelStateAt bytes res)))

voxelStateAt :: V.Vector Word8 -> Int -> Coordinate -> VoxelState
voxelStateAt bytes res Coordinate {..} =
  if testBit (bytes V.! byteIdx) bitIdx
    then Full
    else Void
  where
    offset = cx * res * res + cy * res + cz
    (byteIdx, bitIdx) = offset `divMod` 8
