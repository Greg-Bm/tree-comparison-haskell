{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE Strict #-}

module Tree.Bst (Tree) where

import Control.DeepSeq
import Control.Monad
import Data.Foldable (toList)
import Data.Functor ()
import Data.List hiding (delete, insert, lookup)
import GHC.Generics
import GHC.IO.Exception
import Tree.Zipper
import qualified Tree
import Prelude hiding (delete, lookup)

data Tree a where
  Nil :: Tree a
  Node :: a -> Tree a -> Tree a -> Tree a
  deriving (Eq, Ord, Show, Generic, NFData)

instance Tree.SearchTree Tree where
  emptyTree = Nil
  insert = insert
  delete = delete
  lookup elt = zlookup elt . toZipper

instance Foldable Tree where
  foldr = foldrInorder

data Cxt a = Root | CL a (Cxt a) (Tree a) | CR a (Tree a) (Cxt a)
  deriving (Eq, Ord, Show, Generic, NFData)

data Zipper a = Zipper (Tree a) (Cxt a)

instance BTZipper Zipper where
  valLeftRight (Zipper n cxt) = case n of
    Nil -> Nothing
    Node val left right -> Just (val, Zipper left (CL val cxt right), Zipper right (CR val left cxt))

  up (Zipper n c) = case c of
    Root -> Nothing
    CL a h t -> Just $ Zipper (Node a n t) h
    CR a t h -> Just $ Zipper (Node a t n) h

foldrPreorder :: (t -> c -> c) -> c -> Tree t -> c
foldrPreorder f = flip go
  where
    go Nil = id
    go (Node e l r) = f e . go l . go r

foldrPostorder f = flip go
  where
    go Nil = id
    go (Node e l r) = go l . go r . f e

foldrInorder f = flip go
  where
    go Nil = id
    go (Node e l r) = go l . f e . go r

insert :: (Ord a) => a -> Tree a -> Tree a
insert elt tree = fromZipper $ mapFst ins $ locate elt (toZipper tree)
  where
    ins Nil = Node elt Nil Nil
    ins (Node _ l r) = Node elt l r

delete :: (Ord a) => a -> Tree a -> Tree a
delete elt tree = fromZipper $ deleteCur $ locate elt (toZipper tree)

-- NOTE precondition: all vals in right are greater than all vals in left
mergeLR :: Tree a -> Tree a -> Tree a
mergeLR Nil tree = tree
mergeLR tree Nil = tree
mergeLR left right@(Node re rl rr) = Node elt left right'
  where
    loc@(Zipper tree cxt) = findLeastIn (toZipper right)
    elt = case tree of
      Nil -> re
      Node lre _ _ -> lre
    right' = fromZipper $ deleteCur loc

deleteCur = mapFst del
  where
    del Nil = Nil
    del (Node elt l r) = mergeLR l r


fromZipper = (\(Zipper t c) -> t) . zroot
toZipper f = Zipper f Root


curLeftRight (Node val left right, cxt) = Just (val, (left, CL val cxt right), (right, CR val left cxt))
curLeftRight _ = Nothing

up :: (Tree a, Cxt a) -> Maybe (Tree a, Cxt a)
up (node, CL a cxt right) = Just (Node a node right, cxt)
up (node, CR a left cxt) = Just (Node a left node, cxt)
up _ = Nothing


fromList :: (Ord a) => [a] -> Tree a
fromList = foldl' (flip insert) Nil

mapFst f (Zipper t c) = Zipper (f t) c

toList = foldrInorder (:) []
