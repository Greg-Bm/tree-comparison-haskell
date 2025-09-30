# About

The purpose of this project is to compare a few implementations of tree data structures, including an unbalanced binary search tree and several types of balanced binary search trees. Additionally, a comparison to the tree data structure from `Data.Set` is provided.

# Status

So far, the following types of tree are implemented:
* BST
* AVL Tree

The following operations are supported for trees:
* Insertion
* Deletion
* Lookup
* Inorder traversal

# Tree implementation
Tree operations are implemented via zippers.

In general, the implementation of trees should seek to make invalid states in the code unrepresentable (parse, don't validate). In particular, balance-based invariants of trees, such as the balance invariant for AVL trees, are enforced at the type level.

# Usage

Run the benchmarks with `cabal run benchmark -O2`

To get an html visualization, use `cabal run benchmark -O2 -- --output bench.html`, then open `bench.html` in a browser.
