{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}

module State where

import           Cmd             (Cmd)
import           Data.Map.Strict
import           Data.Set        (Set)
import           Model           (Coordinate, Matrix)

data State =
  State { energy       :: Energy
        , harmonics    :: Harmonics
        , filledVoxels :: Set Coordinate
        , matrix       :: Matrix
        , bots         :: Map BotId Bot
        , trace        :: [Cmd]
        }

newtype Energy = Energy Int deriving (Num)

data Harmonics = Low | High

data Bot =
  Bot { coord :: Coordinate
      , seeds :: [BotId]
      }

newtype BotId = BotId Int deriving (Eq, Ord)

checkForm :: State -> Either String State
checkForm state@State{ .. }
  | groundingMalformed harmonics matrix = Left "All voxels must be grounded if harmonics are low"
    -- Each active nanobot has a different identifier.
    -- The position of each active nanobot is distinct and is Void in the matrix.
    -- The seeds of each active nanobot are disjoint.
    -- The seeds of each active nanobot does not include the identifier of any active nanobot.
  | otherwise = Right state

groundingMalformed :: Harmonics -> Matrix -> Bool
groundingMalformed Low m  = allGrounded m
groundingMalformed High _ = False

allGrounded :: Matrix -> Bool
allGrounded _ = undefined
