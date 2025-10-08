{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module Tree.Rbt () where

import Data.Kind
import Data.Nat
import Tree.Zipper
import Data.Foldable
import Tree

data X a = forall c n. X (Tree c n a)

deriving instance Show a => Show (X a)

data Color = Red | Black
  deriving (Eq, Ord, Show)

data SColor (color :: Color) :: Type where
  SRed :: SColor Red
  SBlack :: SColor Black

deriving instance Show (SColor c)

-- black depth invariant
type family BDepth (color :: Color) (nat :: Nat) :: Nat where
  BDepth Black n = S n
  BDepth Red n = n

-- no red nodes under red nodes
type family Below (root :: Color) (child :: Color) :: Bool where
  Below Red Red = False
  Below Red Black = True
  Below Black a = True

type Valid cc cl cr = (Below cc cl ~ True, Below cc cr ~ True)

data Tree c n a where
  Nil :: Tree Black Z a
  Node ::
    (Valid c cl cr) =>
    SColor c ->
    a ->
    Tree cl n a ->
    Tree cr n a ->
    Tree c (BDepth c n) a

deriving instance (Show e) => Show (Tree c n e)

instance Foldable X where
  foldr f e (X t) = foldrInorder f e t

foldrInorder :: (a -> b -> b) -> b -> Tree c n a -> b
foldrInorder f e t = foldrInorder' f t e

foldrInorder' :: (a -> b -> b) -> Tree c n a -> b -> b
foldrInorder' f Nil = id
foldrInorder' f (Node _ e l r) = foldrInorder' f l . f e . foldrInorder' f r


data TreeColor c n a where
  ItsRed :: Tree Red n a -> TreeColor Red n a
  ItsBlack :: Tree Black n a -> TreeColor Black n a

color :: Tree c n a -> TreeColor c n a
color Nil = ItsBlack Nil
color n@(Node c a l r) = case c of
  SRed -> ItsRed n
  SBlack -> ItsBlack n

data Direction = DLeft | DRight
  deriving (Eq, Ord, Show)

data Context c n a where
  Root :: Context Black n a
  Down ::
    (Valid cp c co) =>
    Direction ->
    SColor cp ->
    a ->
    Tree co n a ->
    Context cp (BDepth cp n) a ->
    Context c n a

cDown ::
  (Valid cp c co) =>
  SColor c ->
  Direction ->
  SColor cp ->
  a ->
  Tree co n a ->
  Context cp (BDepth cp n) a ->
  Context c n a
cDown color = Down

cRed ::
  (Valid cp Red co) =>
  Direction ->
  SColor cp ->
  a ->
  Tree co n a ->
  Context cp (BDepth cp n) a ->
  Context Red n a
cRed = Down

cBlack ::
  (Valid cp Black co) =>
  Direction ->
  SColor cp ->
  a ->
  Tree co n a ->
  Context cp (BDepth cp n) a ->
  Context Black n a
cBlack = Down

data Zipper a = forall c n. Zipper (Tree c n a) (Context c n a)

merge ::
  (Below c cr ~ True, Below c cl ~ True) =>
  Direction ->
  SColor c ->
  a ->
  Tree cr n a ->
  Tree cl n a ->
  Tree c (BDepth c n) a
merge dir col a t other = case dir of
  DLeft -> Node col a t other
  DRight -> Node col a other t

instance BTZipper Zipper where
  up (Zipper t Root) = Nothing
  up (Zipper t (Down dir col a other cxt)) = Just $ Zipper pt cxt
    where
      pt = case dir of
        DLeft -> Node col a t other
        DRight -> Node col a other t

  valLeftRight (Zipper Nil _) = Nothing
  valLeftRight (Zipper (Node color a l r) cxt) =
    Just (a, Zipper l $ Down DLeft color a r cxt, Zipper r $ Down DRight color a l cxt)

instance SearchTree X where
  emptyTree = X Nil
  lookup elt = zlookup elt . toZipper
  insert elt = fromZipper . handle . locate elt . toZipper
    where
      handle z@(Zipper t cxt) = case t of
        Nil -> insert' (Node SRed elt Nil Nil) cxt
        Node c _ l r -> Zipper (Node c elt l r) cxt
  delete elt = error "Not implemented yet."


toZipper :: X a -> Zipper a
toZipper (X Nil) = Zipper Nil Root
toZipper (X (Node _ e l r)) = Zipper (Node SBlack e l r) Root

fromZipper = (\(Zipper t _) -> X t) . zroot

-- adapted from https://gist.github.com/rampion/2659812#file-redblacktree-hs-L7
insert' :: Tree Red n a -> Context c n a -> Zipper a
-- root
insert' (Node SRed e l r) Root = Zipper (Node SBlack e l r) Root
-- black parent
insert' t (Down d SBlack e o p) = Zipper t (Down d SBlack e o p)
-- red uncle, black gparent -> make parent & uncle black, gparent red
insert' t (Down d SRed pe po (Down pd SBlack ge (Node SRed eu lu ru) cxt)) = insert' t' cxt
  where
    u' = Node SBlack eu lu ru
    p' = merge d SBlack pe t po
    t' = merge pd SRed ge p' u'

-- node between parent and gparent. inner rotation
insert' (Node SRed e l r) (Down DLeft SRed pe po cxt@(Down DRight SBlack _ _ _)) =
  insert' (Node SRed pe r po) (cBlack DRight SRed e l cxt)
insert' (Node SRed e l r) (Down DRight SRed pe po cxt@(Down DLeft SBlack _ _ _)) =
  insert' (Node SRed pe po l) (cBlack DLeft SRed e r cxt)

-- node outside. outer rotation
insert' t (Down DLeft SRed pe po (Down DLeft SBlack ge go cxt)) =
  case color go of
    ItsBlack go -> Zipper (Node SBlack pe t (Node SRed ge po go)) cxt

insert' t (Down DRight SRed pe po (Down DRight SBlack ge go cxt)) =
  case color go of
    ItsBlack go -> Zipper (Node SBlack pe (Node SRed ge go po) t) cxt
