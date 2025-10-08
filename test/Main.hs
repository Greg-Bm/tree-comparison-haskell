{-# LANGUAGE BlockArguments #-}

module Main where

import Control.Monad
import Data.Foldable
import Data.Maybe
import Data.Set qualified as Set
import System.Random
import System.Random.Stateful
import Test.Hspec
import Tree
import Tree.Avl as Avl
import Tree.Bst as Bst
import Tree.Rbt as Rbt


main :: IO ()
main = hspec $ do
  smallTest
  largeTest

smallTest = pure ()

largeTest = do
  largeTestTree "BT" (emptyTree :: Bst.Tree Int)
  largeTestTree "AVL" (emptyTree :: Avl.Tree Int)
  largeTestTree "RBT" (emptyTree :: Rbt.Tree Int)
largeTestTree :: (SearchTree t, Foldable t) => String -> t Int -> Spec
largeTestTree name init = do
  let largenum = 10000
  let list1 = genRandomList largenum 1
      list2 = genRandomList largenum 2
      list3 = genRandomList largenum 3
      list12 = list1 <> list2
      list23 = list2 <> list3
  let set1 = Set.fromList list12
      tree1 = foldl' (flip Tree.insert) init list12
      set2 = foldl' (flip Set.delete) set1 list23
      tree2 = foldl' (flip Tree.delete) tree1 list23
  it (name <> " tree traverses correctly after construction") $
    isAscending (toList tree1)
  it (name <> " tree matches reference implementation") $
    toList set1 == toList tree1
  it (name <> " tree lookup finds all elements in a subset") $
    all (`Tree.member` tree1) list1
  it (name <> " tree lookup matches reference implementation") $
    fmap (`Tree.member` tree1) list3 == fmap (`Set.member` set1) list3
  it (name <> " traverses in order after mass deletion") $
    isAscending (toList tree2)
  it (name <> " tree removal matches set") $
    toList set2 == toList tree2
  pure ()

genRandomList :: Int -> Int -> [Int]
genRandomList n seed = runStateGen_ (mkStdGen seed) (replicateM n . uniformRM (1, 5 * n))

isAscending xs = all (uncurry (<=)) (zip xs (drop 1 xs))
