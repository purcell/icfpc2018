{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Trace
  ( toBinaryTrace
  , debugCmdBinary
  , dumpTrace
  , unsafeDumpTrace
  ) where

import Cmd
import Data.Binary.Put
import Data.Bits
import qualified Data.ByteString.Lazy as BSL
import Data.Foldable (traverse_)
import Data.Word
import Geometry (LLD(..), NCD(..), SLD(..), VectorDiff(..))
import System.IO.Unsafe (unsafePerformIO)
import System.Random (randomIO)

dumpTrace :: [Cmd] -> FilePath -> IO ()
dumpTrace !cmds traceFile = BSL.writeFile traceFile (toBinaryTrace cmds)

unsafeDumpTrace :: [Cmd] -> ()
unsafeDumpTrace cmds =
  unsafePerformIO $ do
    n :: Int <- randomIO
    dumpTrace cmds ("/tmp/traces/" ++ show n)

toBinaryTrace :: [Cmd] -> BSL.ByteString
toBinaryTrace cmds = runPut (putCmds cmds)

putCmds :: [Cmd] -> Put
putCmds = traverse_ putCmd

putCmd :: Cmd -> Put
putCmd Halt = putWord8 0xff
putCmd Wait = putWord8 (0xff `xor` 1)
putCmd FlipHarmonics = putWord8 (0xff `xor` 2)
putCmd (SMove lld) = do
  putWord8 (setBit 0 2 .|. shiftL (lldAxis lld) 4)
  putWord8 (lldIncr lld)
putCmd (LMove sld1 sld2) = do
  putWord8 (12 .|. shiftL (sldAxis sld1) 4 .|. shiftL (sldAxis sld2) 6)
  putWord8 (shiftL (sldIncr sld2) 4 .|. sldIncr sld1)
putCmd (FusionP ncd) = putWord8 (7 .|. shiftL (ncdBits ncd) 3)
putCmd (FusionS ncd) = putWord8 (6 .|. shiftL (ncdBits ncd) 3)
putCmd (Fission ncd (SeedAmount seedAmt)) = do
  putWord8 (5 .|. shiftL (ncdBits ncd) 3)
  putWord8 (fromIntegral seedAmt)
putCmd (Fill ncd) = putWord8 (3 .|. shiftL (ncdBits ncd) 3)
putCmd (Void ncd) = putWord8 (2 .|. shiftL (ncdBits ncd) 3)

lldAxis :: LLD -> Word8
lldAxis (LLD VectorDiff {..})
  | dx /= 0 = 1
  | dy /= 0 = 2
  | dz /= 0 = 3
  | otherwise = error "Invalid LLD!"

lldIncr :: LLD -> Word8
lldIncr (LLD VectorDiff {..})
  | dx /= 0 = fromIntegral $ dx + 15
  | dy /= 0 = fromIntegral $ dy + 15
  | dz /= 0 = fromIntegral $ dz + 15
  | otherwise = error "Invalid LLD!"

sldAxis :: SLD -> Word8
sldAxis (SLD VectorDiff {..})
  | dx /= 0 = 1
  | dy /= 0 = 2
  | dz /= 0 = 3
  | otherwise = error "Invalid SLD!"

sldIncr :: SLD -> Word8
sldIncr (SLD VectorDiff {..})
  | dx /= 0 = fromIntegral $ dx + 5
  | dy /= 0 = fromIntegral $ dy + 5
  | dz /= 0 = fromIntegral $ dz + 5
  | otherwise = error "Invalid SLD!"

ncdBits :: NCD -> Word8
ncdBits (NCD VectorDiff {..}) =
  fromIntegral $ (dx + 1) * 9 + (dy + 1) * 3 + (dz + 1)

debugCmdBinary :: Cmd -> String
debugCmdBinary c = show bytes
  where
    bytes = BSL.unpack $ runPut (putCmd c)
