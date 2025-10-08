module Tree.Zipper where

import Control.Monad

class BTZipper z where
  up :: z a -> Maybe (z a)
  valLeftRight :: z a -> Maybe (a, z a, z a)

zroot :: BTZipper z => z a -> z a
zroot = untilNothing up

locate :: (BTZipper z, Ord a) => a -> z a -> z a
locate elt zipper = maybe zipper look (valLeftRight zipper)
  where
    look (val, l, r) = case compare elt val of
      EQ -> zipper
      LT -> locate elt l
      GT -> locate elt r

zlookup elt = res . locate elt
  where
    res = maybe Nothing (\(a,_,_) -> Just a) . valLeftRight

findLeastIn :: (BTZipper z, Ord a) => z a -> z a
findLeastIn = untilNothing (valLeftRight >=> descend)
  where
    descend (_, l, _) = maybe Nothing (\_ -> Just l) (valLeftRight l)

findGreatestIn :: (BTZipper z, Ord a) => z a -> z a
findGreatestIn = untilNothing (valLeftRight >=> descend)
  where
    descend (_, _, r) = maybe Nothing (\_ -> Just r) (valLeftRight r)

-- iterate until Nothing is reached, return second to last value.
untilNothing :: (a -> Maybe a) -> a -> a
untilNothing f = go where go elt = maybe elt go (f elt)
