{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE OverloadedRecordDot, NamedFieldPuns, RecordWildCards #-}
{-# LANGUAGE Strict#-}


module BT.Tree (Tree) where
import Prelude hiding (lookup, delete)
import Data.List hiding (insert, lookup, delete)
import Data.Foldable (toList)
import Control.Monad
import Data.Functor ()
import GHC.IO.Exception
import Control.DeepSeq
import GHC.Generics
import qualified Tree

data Tree a where
  Nil :: Tree a
  Node :: a -> Tree a -> Tree a -> Tree a
  deriving (Eq, Ord, Show, Generic, NFData)

instance Tree.SearchTree Tree where
  emptyTree = Nil
  insert = insert
  delete = delete
  lookup = lookup

instance Foldable Tree where
  foldr = foldrInorder

data Cxt a = Root | CL a (Cxt a) (Tree a) | CR a (Tree a) (Cxt a)
   deriving (Eq, Ord, Show, Generic, NFData)
type Loc a = (Tree a, Cxt a)


foldrPreorder :: (t -> c -> c) -> c -> Tree t -> c
foldrPreorder f = flip go where
  go Nil = id
  go (Node e l r) = f e . go l . go r

foldrPostorder f = flip go where
  go Nil = id
  go (Node e l r) = go l . go r . f e

foldrInorder f = flip go where
  go Nil = id
  go (Node e l r) = go l . f e . go r

lookup elt tree = check . fst $ locate elt (toLoc tree) where
  check Nil = Nothing
  check (Node e _ _) = Just e

insert :: Ord a => a -> Tree a -> Tree a
insert elt tree = fromLoc $ mapFst ins $ locate elt (toLoc tree) where
  ins Nil = Node elt Nil Nil
  ins (Node _ l r) = Node elt l r

delete :: Ord a => a -> Tree a -> Tree a
delete elt tree = fromLoc $ deleteCur $ locate elt (toLoc tree)


-- NOTE precondition: all vals in right are greater than all vals in left
mergeLR :: Tree a -> Tree a -> Tree a
mergeLR Nil tree = tree
mergeLR tree Nil = tree
mergeLR left right@(Node re rl rr) = Node elt left right' where
  loc@(tree, cxt) = locateLeast (toLoc right)
  elt = case tree of
    Nil -> re
    Node lre _ _ -> lre
  right' = fromLoc $ deleteCur loc


deleteCur = mapFst del where
  del Nil = Nil
  del (Node elt l r) = mergeLR l r

locateGreatest :: (Tree a, Cxt a) -> (Tree a, Cxt a)
locateGreatest
 = untilNothing (curLeftRight >=> descend)
  where
    descend (elt, l, r) = case fst r of
      Nil -> Nothing
      right -> Just r

locateLeast
 = untilNothing (curLeftRight >=> descend)
  where
    descend (elt, l, r) = case fst l of
      Nil -> Nothing
      left -> Just l


toLoc tree = (tree, Root)


fromLoc :: (Tree a, Cxt a) -> Tree a
fromLoc = fst . untilNothing up


locate :: Ord p => p -> (Tree p, Cxt p) -> (Tree p, Cxt p)
locate elt = untilNothing (curLeftRight >=> descend) where
   descend (val, l, r) = case compare elt val of
      EQ -> Nothing
      GT -> Just r
      LT -> Just l


curLeftRight (Node val left right, cxt) = Just (val, (left, CL val cxt right), (right, CR val left cxt))
curLeftRight _ = Nothing

up :: (Tree a, Cxt a) -> Maybe (Tree a, Cxt a)
up (node, CL a cxt right) = Just (Node a node right, cxt)
up (node, CR a left cxt) = Just (Node a left node, cxt)
up _ = Nothing

-- iterate until Nothing is reached, return second to last value.
untilNothing :: (a -> Maybe a) -> a -> a
untilNothing f = go where go elt = maybe elt go (f elt)

fromList :: Ord a => [a] -> Tree a
fromList = foldl' (flip insert) Nil

mapFst f (a,b) = (f a, b)

toList = foldrInorder (:) []
