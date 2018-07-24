{-# LANGUAGE RecordWildCards #-}

module Model where

import Data.Binary.Get
import Data.Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Vector as V
import Data.Word
import Geometry (Coordinate(..))
import Matrix

modelFromFile :: FilePath -> IO Matrix
modelFromFile path = runGet getMatrix <$> BSL.readFile path

getMatrix :: Get Matrix
getMatrix = do
  res <- fromIntegral <$> getWord8
  let bytesToRead = ceiling (fromIntegral (res * res * res) / 8 :: Rational)
  bytes <- V.fromList . BS.unpack <$> getByteString bytesToRead
  let filledCoords =
        [ c
        | x <- [1 .. res - 2]
        , y <- [0 .. res - 2]
        , z <- [1 .. res - 2]
        , let c = Coordinate x y z
        , voxelFilledAt bytes res c
        ]
  return (makeMatrix res filledCoords)

voxelFilledAt :: V.Vector Word8 -> Int -> Coordinate -> Bool
voxelFilledAt bytes res Coordinate {..} = testBit (bytes V.! byteIdx) bitIdx
  where
    offset = cx * res * res + cy * res + cz
    (byteIdx, bitIdx) = offset `divMod` 8
