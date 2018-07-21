module UpdateSpec (main, spec) where

import           Test.Hspec
import           Test.QuickCheck

import           Cmd
import           Data.Map.Strict as Map
import qualified Data.Set        as Set
import           Model
import           State           hiding (initialState)
import qualified State
import           Update

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

initialState :: State
initialState =
  State.initialState
    Matrix { matrixResolution = 50
           , matrixFilledVoxels = Set.fromList [Coordinate { cx = 1, cy = 0, cz = 0 }, Coordinate { cx = 0, cy = 5, cz = 0 }]
           }

spec :: Spec
spec =
  describe "performCommand" $ do
    describe "Halt" $
      it "is unimplemented" $
        True === True

    describe "Wait" $
      it "returns the state with the wait command appended to the trace" $
        performCommand (initialBotId, Cmd.Wait) initialState
          `shouldBe` Just initialState { trace = [Cmd.Wait] }

    describe "FlipHarmonics" $
      it "flips dem harmonics" $
         harmonics <$> performCommand (initialBotId, Cmd.FlipHarmonics) initialState
          `shouldBe` Just High

    describe "SMove" $ do
      let lld = LLD VectorDiff { dx = 5, dy = 0, dz = 0 }
          maybeState = performCommand (initialBotId, Cmd.SMove lld) initialState

      it "moves the bot by the specified vector" $ do
        let expectedBot = initialBot { coord = Coordinate { cx = 5, cy = 0, cz = 0 } }

        bots <$> maybeState `shouldBe` (Just $ Map.fromList [ (initialBotId, expectedBot) ])

      it "adjusts the energy to reflect that a move was made" $
        -- energy for an SMove = energy + 2 * mlen(lld)
        -- mlen(d) is defined as |dx| + |dy| + |dz| (the sum of the absolute values of dx, dy, and dz)
        energy <$> maybeState `shouldBe` Just 10

    describe "LMove" $ do
      let sld1 = SLD VectorDiff { dx = 0, dy = 3, dz = 0 }
          sld2 = SLD VectorDiff { dx = 0, dy = 0, dz = 7 }
          maybeState = performCommand (initialBotId, Cmd.LMove sld1 sld2) initialState

      it "moves the bot by the specified vector" $ do
        let expectedBot = initialBot { coord = Coordinate { cx = 0, cy = 3, cz = 7 } }

        bots <$> maybeState `shouldBe` (Just $ Map.fromList [ (initialBotId, expectedBot) ])

      it "adjusts the energy to reflect that a move was made" $
        -- Energy for LMove = energy + 2 * (mlen(sld1) + 2 + mlen(sld2))
        -- mlen(d) is defined as |dx| + |dy| + |dz| (the sum of the absolute values of dx, dy, and dz)
        energy <$> maybeState `shouldBe` Just 24

    describe "Fill" $ do
      let ncd = NCD VectorDiff { dx = 1, dy = 0, dz = 0 }
          maybeState = performCommand (initialBotId, Cmd.Fill ncd) initialState

      it "fills the voxel at the specified vector" $ do
        let expectedCoord = Coordinate { cx = 1, cy = 0, cz = 0 }

        ((flip isFilled) expectedCoord . matrix) <$> maybeState `shouldBe` Just True

      it "does not fill the voxel at the specified vector if it is not filled in the target" $ do
        let ncd = NCD VectorDiff { dx = 1, dy = 0, dz = 2 }
            expectedCoord = Coordinate { cx = 1, cy = 0, cz = 2 }
            maybeState = performCommand (initialBotId, Cmd.Fill ncd) initialState

        ((flip isFilled) expectedCoord . matrix) <$> maybeState `shouldBe` Just False

      it "does not fill the voxel at the specified vector if it is not grounded" $ do
        let ncd = NCD VectorDiff { dx = 0, dy = 5, dz = 0 }
            expectedCoord = Coordinate { cx = 0, cy = 5, cz = 0 }
            maybeState = performCommand (initialBotId, Cmd.Fill ncd) initialState

        ((flip isFilled) expectedCoord . matrix) <$> maybeState `shouldBe` Just False

      it "adjusts the energy by 12 when the specified voxel was Void" $
        energy <$> maybeState `shouldBe` Just 12

      it "adjusts the energy by 6 when the specified voxel was Full" $
        energy <$> (maybeState >>= performCommand (initialBotId, Cmd.Fill ncd))
          `shouldBe` Just 18 -- 12 for the first Fill, 6 for the second
