module Astar where

import Data.Foldable (foldl')
import qualified Data.IntMap as IntMap
import Data.IntMap (IntMap)
import qualified Data.Set as Set

-- Ints are cost and heuristic (distance from completion)
astar :: Ord a => (a -> [(a, Int, Int)]) -> a -> [(a, Int)]
astar = astarOn id

astarOn :: Ord b => (a -> b) -> (a -> [(a, Int, Int)]) -> a -> [(a, Int)]
astarOn rep nexts start = go Set.empty (IntMap.singleton 0 [(0, start)])
  where
    go seen work =
      case pqueueNext work of
        Nothing -> []
        Just ((cost, x), work1)
          | Set.member r seen -> go seen work1
          | otherwise -> (x, cost) : go seen' work2
          where r = rep x
                seen' = Set.insert r seen
                work2 = foldl' addWork work1 (nexts x)
                addWork w (x', stepcost, heuristic) =
                  let cost' = cost + stepcost
                   in cost' `seq` pqueuePush (cost' + heuristic) (cost', x') w

pqueuePush :: Int -> a -> IntMap [a] -> IntMap [a]
pqueuePush k v = IntMap.alter aux k
  where
    aux Nothing = Just [v]
    aux (Just vs) = Just (v : vs)

pqueueNext :: IntMap [a] -> Maybe (a, IntMap [a])
pqueueNext q = do
  ((k, xs), q1) <- IntMap.minViewWithKey q
  case xs of
    [] -> error "Malformed queue"
    [x] -> Just (x, q1)
    x:rest ->
      let q2 = IntMap.insert k rest q1
       in q2 `seq` Just (x, q2)
