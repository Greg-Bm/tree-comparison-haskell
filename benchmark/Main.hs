module Main where

import System.Random
import System.Random.Stateful
import Criterion.Main
import Data.Set as Set
import Tree as Tree
import Tree.Avl as Avl
import Tree.Bst as Bst
import Control.Monad


main :: IO ()
main = defaultMain [
  bgroup "build" [ bench "set e3" $ nf Set.fromList liste3
                 , bench "avl e3" $ nf (Tree.fromList :: [Int] -> Avl.Tree Int) liste3
                 , bench "bst e3" $ nf (Tree.fromList :: [Int] -> BST.Tree Int) liste3
                 , bench "set e4" $ nf Set.fromList liste4
                 , bench "avl e4" $ nf (Tree.fromList :: [Int] -> Avl.Tree Int) liste4
                 , bench "bst e4" $ nf (Tree.fromList :: [Int] -> BST.Tree Int) liste4
                 , bench "set e5" $ nf Set.fromList liste5
                 , bench "avl e5" $ nf (Tree.fromList :: [Int] -> Avl.Tree Int) liste5
                 , bench "bst e5" $ nf (Tree.fromList :: [Int] -> BST.Tree Int) liste5
                 ]
  ]

liste3 = genRandomList 1000 1
liste4 = genRandomList 10000 1
liste5 = genRandomList 100000 1

genRandomList :: Int -> Int -> [Int]
genRandomList n seed  = runStateGen_ (mkStdGen seed) (replicateM n . uniformRM (1, billion))

billion = 1000000000
