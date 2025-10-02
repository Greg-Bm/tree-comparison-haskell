{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE BangPatterns #-}
module Tree.Workloads (buildAndQuery, mixedQueries, Action(..)) where
import Data.Proxy
import Tree as Tree
import Data.Foldable
import GHC.Generics
import Control.DeepSeq

buildAndQuery :: (SearchTree t, Ord a) => Proxy (t a) -> [a] -> [a] -> ([a], [Bool])
buildAndQuery proxy contents queries = do
  let tree = asProxyTypeOf (Tree.fromList contents) proxy
      queryResults = (`Tree.member` tree) <$> queries
  (toList tree, queryResults)

data Action = Insert | Delete | Member
  deriving (Eq, Ord, Show, Generic, NFData)

mixedQueries :: (SearchTree t, Ord a) => Proxy (t a) -> [(Action, a)] -> ([Bool], t a)
mixedQueries proxy queries =
  process queries [] (asProxyTypeOf Tree.emptyTree proxy)
        where
          process [] out t = (out, t)
          process ((q, e) : rest) !out t = process rest (x : out) t'
            where (x, t') = case q of
                    Insert -> (True, insert e t)
                    Delete -> (True, delete e t)
                    Member -> (member e t, t)
