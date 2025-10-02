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
    [ mixedQueriesTest 100000 100000 1
    , mixedQueriesTest 100000 1000 1
    ]

mixedQueriesTest length range seed =
  env (pure (mixedQueriesInput length range seed)) $ \list ->
    bgroup ("length " <> show length <> ", range " <> show range) $
      [ bench "set" $ nf (mixedQueries @Set.Set Proxy) list,
        bench "avl" $ nf (mixedQueries @Avl.Tree Proxy) list,
        bench "bst" $ nf (mixedQueries @Bst.Tree Proxy) list
      ]

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
