{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Monad
import Criterion.Main
import Data.Proxy
import Data.Set as Set
import System.Random
import System.Random.Stateful
import Tree
import Tree.Avl as Avl
import Tree.Bst as Bst
import Tree.Workloads

main :: IO ()
main =
  defaultMain
    [ mixedQueriesTests 1000 1000 100,
      mixedQueriesTests 1000 20 100,
      mixedQueriesTests 10000 10000 100
    ]

mixedQueriesTests :: Int -> Int -> Int -> Benchmark
mixedQueriesTests length range trials =
  env (pure (mixedQueriesTestInput length range trials)) $ \list ->
    bgroup ("length " <> show length <> ", range " <> show range <> ", trials " <> show trials) [ bench "set" $ nf (mixedQueriesTest @Set.Set Proxy) list,
        bench "avl" $ nf (mixedQueriesTest @Avl.Tree Proxy) list,
        bench "bst" $ nf (mixedQueriesTest @Bst.Tree Proxy) list
      ]

mixedQueriesTest :: (SearchTree t, Ord a) => Proxy (t a) -> [[(Action, a)]] -> [([Bool], t a)]
mixedQueriesTest proxy = fmap (mixedQueries proxy)

mixedQueriesTestInput :: Int -> Int -> Int -> [[(Action, Int)]]
mixedQueriesTestInput length range n = mixedQueriesInput length range <$> [1 .. n]

mixedQueriesInput :: Int -> Int -> Int -> [(Action, Int)]
mixedQueriesInput length range seed = do
  let nums = genRandomList length range seed
      actions = numToAction <$> genRandomList length 3 seed
      numToAction = \case
        1 -> Insert
        2 -> Delete
        3 -> Member
  zip actions nums

genRandomList :: Int -> Int -> Int -> [Int]
genRandomList length range seed = runStateGen_ (mkStdGen seed) (replicateM length . uniformRM (1, range))
