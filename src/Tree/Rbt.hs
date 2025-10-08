{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wincomplete-patterns #-}

module Tree.Rbt (Tree) where

import Data.Foldable
import Data.Kind
import Data.Nat
import Tree
import Tree.Zipper
import Control.DeepSeq


data Tree a = forall c n. Tree (T c n a)

instance NFData (Tree a) where
  rnf a = ()

deriving instance (Show a) => Show (Tree a)

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
  Below root child = True

type Valid cc cl cr = (Below cc cl ~ True, Below cc cr ~ True)

data T c n a where
  Nil :: T Black Z a
  Node ::
    (Valid c cl cr) =>
    SColor c ->
    a ->
    T cl n a ->
    T cr n a ->
    T c (BDepth c n) a

deriving instance (Show e) => Show (T c n e)

instance Foldable Tree where
  foldr f e (Tree t) = foldrInorder f e t

foldrInorder :: (a -> b -> b) -> b -> T c n a -> b
foldrInorder f e t = foldrInorder' f t e

foldrInorder' :: (a -> b -> b) -> T c n a -> b -> b
foldrInorder' f Nil = id
foldrInorder' f (Node _ e l r) = foldrInorder' f l . f e . foldrInorder' f r

data TreeColor c n a where
  ItsRed :: T Red n a -> TreeColor Red n a
  ItsBlack :: T Black n a -> TreeColor Black n a

color :: T c n a -> TreeColor c n a
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
    T co n a ->
    Context cp (BDepth cp n) a ->
    Context c n a

cDown ::
  (Valid cp c co) =>
  SColor c ->
  Direction ->
  SColor cp ->
  a ->
  T co n a ->
  Context cp (BDepth cp n) a ->
  Context c n a
cDown color = Down

cRed ::
  (Valid cp Red co) =>
  Direction ->
  SColor cp ->
  a ->
  T co n a ->
  Context cp (BDepth cp n) a ->
  Context Red n a
cRed = Down

cBlack ::
  (Valid cp Black co) =>
  Direction ->
  SColor cp ->
  a ->
  T co n a ->
  Context cp (BDepth cp n) a ->
  Context Black n a
cBlack = Down

data Zipper a = forall c n. Zipper (T c n a) (Context c n a)

merge ::
  (Below c cr ~ True, Below c cl ~ True) =>
  Direction ->
  SColor c ->
  a ->
  T cr n a ->
  T cl n a ->
  T c (BDepth c n) a
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

instance SearchTree Tree where
  emptyTree = Tree Nil
  lookup elt = zlookup elt . toZipper
  insert elt = fromZipper . handle . locate elt . toZipper
    where
      handle z@(Zipper t cxt) = case t of
        Nil -> insert' (Node SRed elt Nil Nil) cxt
        Node c _ l r -> Zipper (Node c elt l r) cxt
  delete elt = fromZipper . deleteAt . locate elt . toZipper

toZipper :: Tree a -> Zipper a
toZipper (Tree Nil) = Zipper Nil Root
toZipper (Tree (Node _ e l r)) = Zipper (Node SBlack e l r) Root

fromZipper = (\(Zipper t _) -> Tree t) . zroot

-- adapted from https://gist.github.com/rampion/2659812#file-redblacktree-hs-L7
insert' :: T Red n a -> Context c n a -> Zipper a
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

deleteAt :: Ord a => Zipper a -> Zipper a
deleteAt (Zipper Nil cxt) = Zipper Nil cxt
deleteAt (Zipper (Node SBlack _ Nil (Node SRed e Nil Nil)) (Down dir ccol ce co cc)) =
  Zipper
    (Node SBlack e Nil Nil)
    (cBlack dir ccol ce co cc)
deleteAt (Zipper (Node SBlack _ (Node SRed e Nil Nil) Nil) cxt) =
  Zipper
    (Node SBlack e Nil Nil) $ case cxt of
                                Root -> Root
                                (Down dir ccol ce co cc) -> (Down dir ccol ce co cc)

deleteAt (Zipper (Node _ _e Nil Nil) Root) = Zipper Nil Root

deleteAt (Zipper (Node SRed _e Nil Nil) (Down dir col e o cxt)) = Zipper Nil (cBlack dir col e o cxt)
deleteAt (Zipper n@(Node SBlack _e Nil Nil) cxt) = deleteBal Nil cxt


deleteAt z@(Zipper (Node col e l r@(Node _ _ _ _)) cxt) = do
  let (Just (_, _, r')) = valLeftRight z
  let least = findLeastIn r'
  case least of
    Zipper n@(Node _ e' _ _) cxt -> deleteAt $ Zipper n (replaceUp e e' cxt)
    Zipper Nil cxt -> error "should not happen: r is nonempty"

replaceUp :: (Eq b) => b -> b -> Context c n b -> Context c n b
replaceUp _ _ Root = error "did not find cv"
replaceUp cv nv (Down d col val t c)
  | val == cv = Down d col nv t c
  | otherwise = Down d col val t (replaceUp cv nv c)

deleteBal :: T Black n a -> Context c (S n) a -> Zipper a
-- 1. root
deleteBal n Root = Zipper n Root
-- 2. all black. move on to parent
deleteBal n (Down DLeft SBlack pe (Node SBlack se (color -> ItsBlack c) (color -> ItsBlack d)) cxt) = deleteBal (Node SBlack pe n (Node SRed se c d)) cxt
deleteBal n (Down DRight SBlack pe (Node SBlack se (color -> ItsBlack d) (color -> ItsBlack c)) cxt) = deleteBal (Node SBlack pe (Node SRed se d c) n) cxt
-- 3. red sibling. rearrange and retry
deleteBal n (Down DLeft SBlack pe (Node SRed se (color -> ItsBlack c) (color -> ItsBlack d)) cxt) = deleteBal n (cBlack DLeft SRed pe c (Down DLeft SBlack se d cxt))
deleteBal n (Down DRight SBlack pe (Node SRed se (color -> ItsBlack d) (color -> ItsBlack c)) cxt) = deleteBal n (cBlack DRight SRed pe c (Down DRight SBlack se d cxt))
-- 4. red parent. terminate
deleteBal n (Down DLeft SRed pe (Node SBlack se (color -> ItsBlack c) (color -> ItsBlack d)) (Down dir col e o cxt)) =
  Zipper (Node SBlack pe n (Node SRed se c d)) (cBlack dir col e o cxt)
deleteBal n (Down DRight SRed pe (Node SBlack se (color -> ItsBlack d) (color -> ItsBlack c)) (Down dir col e o cxt)) =
  Zipper (Node SBlack pe (Node SRed se d c) n) (cBlack dir col e o cxt)
-- 5. red close nephew. convert and go to case 6.
deleteBal n (Down DLeft SBlack pe (Node SBlack se (Node SRed ce n2 n3) (color -> ItsBlack d)) cxt) =
  deleteBal n (Down DLeft SBlack pe (Node SBlack ce n2 (Node SRed se n3 d)) cxt)
deleteBal n (Down DRight SBlack pe (Node SBlack se (color -> ItsBlack d) (Node SRed ce n3 n2)) cxt) =
  deleteBal n (Down DRight SBlack pe (Node SBlack ce (Node SRed se d n3) n2) cxt)
deleteBal n (Down DLeft SRed pe (Node SBlack se (Node SRed ce n2 n3) (color -> ItsBlack d)) cxt) =
  deleteBal n (cBlack DLeft SRed pe (Node SBlack ce n2 (Node SRed se n3 d)) cxt)
deleteBal n (Down DRight SRed pe (Node SBlack se (color -> ItsBlack d) (Node SRed ce n3 n2)) cxt) =
  deleteBal n (cBlack DRight SRed pe (Node SBlack ce (Node SRed se d n3) n2) cxt)
-- 6. red far nephew.
deleteBal n (Down DLeft pc pe (Node SBlack se c(Node SRed de dl dr)) cxt) =
  Zipper (Node pc se (Node SBlack pe n c) (Node SBlack de dl dr)) cxt
deleteBal n (Down DRight pc pe (Node SBlack se (Node SRed de dl dr) c) cxt) =
  Zipper (Node pc se (Node SBlack de dl dr) (Node SBlack pe c n)) cxt
