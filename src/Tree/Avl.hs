{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

{-# LANGUAGE Strict#-}
{-# LANGUAGE DeriveFoldable #-}

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DataKinds #-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies#-}

module Tree.Avl (Tree, insert, delete, lookup) where
import Prelude hiding (lookup, insert)
import Prelude as P hiding (lookup)
import Data.Nat as N
import Data.Kind
import Data.List(foldl')
import Data.Foldable(toList)
import Data.Void (Void)
import Control.Monad
import Control.DeepSeq
import qualified Tree

import Control.Exception


data Tree key = forall (level :: Nat). Tree (T level key)

instance NFData (Tree k) where
  rnf a = ()

deriving instance (Show key) => Show (Tree key)

instance Foldable Tree where
  foldr f i (Tree t) = foldrInorder f i t


instance Tree.SearchTree Tree where
  emptyTree = emptyTree
  insert = insert
  delete = delete
  lookup = lookup

data Balance (nh :: Nat) nl ng :: Type where
  BE :: Balance (S n) n n
  BL :: Balance (S (S n)) (S n) n
  BR :: Balance (S (S n)) n (S n)

deriving instance Show (Balance a b c)

data T (level :: Nat) (key :: Type) where
  Nil :: T Z key
  Node :: Balance nh nl nr -> key -> T nl key -> T nr key -> T nh key

foldrInorder f i t = foldrInorder' f t i

foldrInorder' :: (a -> b -> b) -> T n a -> b -> b
foldrInorder' f Nil = id
foldrInorder' f (Node _ v l r) =  foldrInorder' f l . f v . foldrInorder' f r
deriving instance (Show key) =>Show (T level key)
deriving instance Foldable (T level)

data Context :: Nat -> Type -> Type where
  Root :: Context n a
  CL :: Balance nh nl nr -> a -> Context nh a -> T nr a -> Context nl a
  CR :: Balance nh nl nr -> a -> T nl a -> Context nh a -> Context nr a

data Zipper a = forall n. Zipper (T n a) (Context n a)


emptyTree = Tree Nil

up :: Zipper a -> Maybe (Zipper a)
up (Zipper t (CL bal elt cxt right)) = Just $ Zipper (Node bal elt t right) cxt
up (Zipper t (CR bal elt left cxt)) = Just $ Zipper (Node bal elt left t) cxt
up (Zipper _ Root) = Nothing


fromZipper = (\(Zipper t _) -> Tree t) . untilNothing up
toZipper (Tree t) = Zipper t Root


valLeftRight (Zipper Nil _) = Nothing
valLeftRight (Zipper (Node bal elt l r) cxt) =
  Just (elt, Zipper l (CL bal elt cxt r), Zipper r (CR bal elt l cxt) )

locate :: Ord a => a -> Zipper a -> Zipper a
locate elt zipper = maybe zipper look (valLeftRight zipper) where
  look (val, l, r) = case compare elt val of
    EQ -> zipper
    LT -> locate elt l
    GT -> locate elt r

lookup elt = res . locate elt . toZipper where
  res (Zipper Nil _) = Nothing
  res (Zipper (Node bal key l r) _) = Just key
insert elt = fromZipper . insert' elt . toZipper
delete elt = fromZipper . delete' elt . toZipper

insert' elt zip = case locate elt zip of
  Zipper Nil cxt -> insertAndFix (Node BE elt Nil Nil) cxt
  Zipper (Node bal key l r) cxt -> Zipper (Node bal elt l r) cxt

delete' :: Ord a => a -> Zipper a -> Zipper a
delete' elt zip = case locate elt zip of
  Zipper Nil cxt -> zip
  Zipper (Node BE _ Nil Nil) cxt -> deleteAndFix Nil cxt
  Zipper (Node BL _ l@Node{} Nil) cxt -> deleteAndFix l cxt
  Zipper (Node BR _ Nil r@Node{}) cxt -> deleteAndFix r cxt
  z@(Zipper (Node bal cv l r) cxt) -> do
    let Just (v, l', r') = valLeftRight z -- this is safe.
    let nzipper = findLeastIn r'
    case nzipper of
      Zipper Nil cxt -> error "this should be impossible."
      Zipper (Node BE v rl rr) cxt -> deleteAndFix rr $ replaceUp cv v cxt
      Zipper (Node BR v rl rr) cxt -> deleteAndFix rr $ replaceUp cv v cxt


findLeastIn :: Zipper a -> Zipper a
findLeastIn = untilNothing (valLeftRight >=> descend) where
  descend (_, l, _) = case l of
    (Zipper Nil _) -> Nothing
    other -> Just l


deleteAndFix :: T n a -> Context (S n) a -> Zipper a
deleteAndFix node Root = Zipper node Root
deleteAndFix node (CR bal val l ctx) = case bal of
  BR -> deleteAndFix (Node BE val l node) ctx
  BE -> Zipper (Node BL val l node) ctx
  BL -> case balanceL val l node of
    Left node' -> deleteAndFix node' ctx
    Right node' ->  Zipper node' ctx
deleteAndFix node (CL bal val ctx r) = case bal of
  BL -> deleteAndFix (Node BE val node r) ctx
  BE -> Zipper (Node BR val node r) ctx
  BR -> case balanceR val node r of
    Left node' -> deleteAndFix node' ctx
    Right node' ->  Zipper node' ctx


replaceUp :: Eq b =>  b -> b -> Context a b -> Context a b
replaceUp _ _ Root = error "did not find cv"
replaceUp cv nv (CL bal val cl r) | val == cv = CL bal val cl r
                                  | otherwise = CL bal val (replaceUp cv nv cl) r
replaceUp cv nv (CR bal val l cr) | val == cv = CR bal nv l cr
                                  | otherwise = CR bal val l (replaceUp cv nv cr)


insertAndFix :: T (S n) a -> Context n a -> Zipper a
insertAndFix node Root = Zipper node Root
insertAndFix node (CR bal val l ctx) = case bal of
  BL -> Zipper (Node BE val l node) ctx
  BE -> insertAndFix (Node BR val l node) ctx
  BR -> case balanceR val l node of
    Left node' -> Zipper node' ctx
    Right node' -> insertAndFix node' ctx
insertAndFix node (CL bal val ctx r) = case bal of
  BR -> Zipper (Node BE val node r) ctx
  BE -> insertAndFix (Node BL val node r) ctx
  BL -> case balanceL val node r of
    Left node' -> Zipper node' ctx
    Right node' -> insertAndFix node' ctx

balanceR :: a -> T n a ->T (S (S n)) a -> Either (T (S (S n)) a) (T (S (S (S n))) a)
balanceR v l (Node rb rv rl rr) =  case rb of
      BR -> Left (Node BE rv (Node BE v l rl) rr)
      BE -> Right (Node BL rv (Node BR v l rl) rr)
      BL -> case rl of
        Node rlb rlv rll rlr -> (\(newl, newr) -> Left (Node BE rlv newl newr)) $ case rlb of
          BL -> (Node BE v l rll, Node BR rv rlr rr)
          BE -> (Node BE v l rll, Node BE rv rlr rr)
          BR -> (Node BL v l rll, Node BE rv rlr rr)

balanceL :: a -> T (S (S n)) a -> T n a -> Either (T (S (S n)) a) (T (S (S (S n))) a)
balanceL v (Node lb lv ll lr) r=  case lb of
      BL -> Left (Node BE lv ll (Node BE v lr r))
      BE -> Right (Node BR lv ll (Node BL v lr r))
      BR -> case lr of
        Node lrb lrv lrl lrr -> (\(newl, newr) -> Left (Node BE lrv newl newr)) $ case lrb of
          BL -> (Node BE lv ll lrl, Node BR v lrr r)
          BE -> (Node BE lv ll lrl, Node BE v lrr r)
          BR -> (Node BL lv ll lrl, Node BE v lrr r)

-- iterate until Nothing is reached, return second to last value.
untilNothing :: (a -> Maybe a) -> a -> a
untilNothing f = go where go elt = maybe elt go (f elt)

