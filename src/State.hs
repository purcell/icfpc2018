{-# LANGUAGE RecordWildCards #-}

module State where

import           Cmd   (Cmd)
import           Model (Coordinate, Matrix)

data State =
  State { energy    :: Energy
        , harmonics :: Harmonics
        , matrix    :: Matrix
        , bots      :: [Bot]
        , trace     :: [Cmd]
        }

newtype Energy = Energy Int

data Harmonics = Low | High

data Bot =
  Bot { bid   :: BotId
      , coord :: Coordinate
      , seeds :: [BotId]
      }

newtype BotId = BotId Int

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
