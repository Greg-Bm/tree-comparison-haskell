module Tree (SearchTree(..),fromList, member) where
import Data.Maybe
import Data.Foldable
import Data.List(foldl')
class SearchTree t where
  emptyTree :: t k
  -- | return value if key is in container, else Nothing
  lookup :: Ord k =>k -> t k -> Maybe k
  -- | insert value at key. lookup k (insert k v t) == Just v
  -- note that an existing value is overwritten.
  insert :: Ord k => k -> t k -> t k
  -- | delete the value. lookup k (delete k v t) == Nothing
  delete :: Ord k => k -> t k -> t k

fromList :: SearchTree t => Ord a => [a] -> t a
fromList = foldl' (flip insert) emptyTree

member :: (SearchTree t, Ord a) => a -> t a -> Bool
member v tree = isJust (Tree.lookup v tree)


