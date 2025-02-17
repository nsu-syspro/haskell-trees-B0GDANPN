{-# OPTIONS_GHC -Wall #-}
-- The above pragma enables all warnings

module Task2 where

-- Explicit import of Prelude to hide functions
-- that are not supposed to be used in this assignment
import Prelude hiding (compare, foldl, foldr, Ordering(..))

import Task1 (Tree(..))

-- * Type definitions

-- | Ordering enumeration
data Ordering = LT | EQ | GT
  deriving (Show, Eq)

-- | Binary comparison function indicating whether first argument is less, equal or
-- greater than the second one (returning 'LT', 'EQ' or 'GT' respectively)
type Cmp a = a -> a -> Ordering

-- * Function definitions

-- | Binary comparison function induced from `Ord` constraint
--
-- Usage example:
--
-- >>> compare 2 3
-- LT
-- >>> compare 'a' 'a'
-- EQ
-- >>> compare "Haskell" "C++"
-- GT
--
compare :: Ord a => Cmp a
compare x y
  | x < y     = LT
  | x > y     = GT
  | otherwise = EQ

-- | Conversion of list to binary search tree
-- using given comparison function
--
-- Usage example:
--
-- >>> listToBST compare [2,3,1]
-- Branch 2 (Branch 1 Leaf Leaf) (Branch 3 Leaf Leaf)
-- >>> listToBST compare ""
-- Leaf
--
listToBST :: Cmp a -> [a] -> Tree a
listToBST _ [] = Leaf
listToBST cmp (x:xs) = tinsert cmp x (listToBST cmp xs)
-- | Conversion from binary search tree to list
--
-- Resulting list will be sorted
-- if given tree is valid BST with respect
-- to some 'Cmp' comparison.
--
-- Usage example:
--
-- >>> bstToList (Branch 2 (Branch 1 Leaf Leaf) (Branch 3 Leaf Leaf))
-- [1,2,3]
-- >>> bstToList Leaf
-- []
--
bstToList :: Tree a -> [a]
bstToList Leaf = []
bstToList (Branch val left right) = bstToList left ++ [val] ++ bstToList right

-- | Tests whether given tree is a valid binary search tree
-- with respect to given comparison function
--
-- Usage example:
--
-- >>> isBST compare (Branch 2 (Branch 1 Leaf Leaf) (Branch 3 Leaf Leaf))
-- True
-- >>> isBST compare (Leaf :: Tree Char)
-- True
-- >>> isBST compare (Branch 5 (Branch 1 Leaf Leaf) (Branch 3 Leaf Leaf))
-- False
--
isBST :: Cmp a -> Tree a -> Bool
isBST cmp = go Nothing Nothing
  where
    go _ _ Leaf = True
    go minVal maxVal (Branch val left right) =
      withinBounds minVal maxVal val &&
      go minVal (Just val) left &&
      go (Just val) maxVal right
    withinBounds minVal maxVal val =
      maybe True (`lt` val) minVal &&
      maybe True (`gt` val) maxVal
    lt x y = cmp x y == LT
    gt x y = cmp x y == GT
-- | Searches given binary search tree for
-- given value with respect to given comparison
--
-- Returns found value (might not be the one that was given)
-- wrapped into 'Just' if it was found and 'Nothing' otherwise.
--
-- Usage example:
--
-- >>> tlookup compare 2 (Branch 2 (Branch 1 Leaf Leaf) (Branch 3 Leaf Leaf))
-- Just 2
-- >>> tlookup compare 'a' Leaf
-- Nothing
-- >>> tlookup (\x y -> compare (x `mod` 3) (y `mod` 3)) 5 (Branch 2 (Branch 0 Leaf Leaf) (Branch 2 Leaf Leaf))
-- Just 2
--
tlookup :: Cmp a -> a -> Tree a -> Maybe a
tlookup _ _ Leaf = Nothing
tlookup cmp x (Branch val left right) =
  case cmp x val of
    LT -> tlookup cmp x left
    GT -> tlookup cmp x right
    EQ -> Just val
-- | Inserts given value into given binary search tree
-- preserving its BST properties with respect to given comparison
--
-- If the same value with respect to comparison
-- was already present in the 'Tree' then replaces it with given value.
--
-- Usage example:
--
-- >>> tinsert compare 0 (Branch 2 (Branch 1 Leaf Leaf) (Branch 3 Leaf Leaf))
-- Branch 2 (Branch 1 (Branch 0 Leaf Leaf) Leaf) (Branch 3 Leaf Leaf)
-- >>> tinsert compare 1 (Branch 2 (Branch 1 Leaf Leaf) (Branch 3 Leaf Leaf))
-- Branch 2 (Branch 1 Leaf Leaf) (Branch 3 Leaf Leaf)
-- >>> tinsert compare 'a' Leaf
-- Branch 'a' Leaf Leaf
--
tinsert :: Cmp a -> a -> Tree a -> Tree a
tinsert _ x Leaf = Branch x Leaf Leaf
tinsert cmp x (Branch val left right) =
  case cmp x val of
    LT -> Branch val (tinsert cmp x left) right
    GT -> Branch val left (tinsert cmp x right)
    EQ -> Branch x left right

-- | Deletes given value from given binary search tree
-- preserving its BST properties with respect to given comparison
--
-- Returns updated 'Tree' if the value was present in it;
-- or unchanged 'Tree' otherwise.
--
-- Usage example:
--
-- >>> tdelete compare 1 (Branch 2 (Branch 1 Leaf Leaf) (Branch 3 Leaf Leaf))
-- Branch 2 Leaf (Branch 3 Leaf Leaf)
-- >>> tdelete compare 'a' Leaf
-- Leaf
--
tdelete :: Cmp a -> a -> Tree a -> Tree a
tdelete _ _ Leaf = Leaf
tdelete cmp x (Branch val left right) =
  case cmp x val of
    LT -> Branch val (tdelete cmp x left) right
    GT -> Branch val left (tdelete cmp x right)
    EQ -> deleteRoot (Branch val left right)


deleteRoot :: Tree a -> Tree a
deleteRoot Leaf = Leaf
deleteRoot (Branch _ Leaf rightSubtree) = rightSubtree
deleteRoot (Branch _ leftSubtree Leaf) = leftSubtree
deleteRoot (Branch _ leftSubtree rightSubtree) =
  let (minRight, newRightSubtree) = extractMin rightSubtree
  in Branch minRight leftSubtree newRightSubtree

extractMin :: Tree a -> (a, Tree a)
extractMin Leaf = error "not exist min on empty tree"
extractMin (Branch value Leaf rightSubtree) = (value, rightSubtree)
extractMin (Branch value leftSubtree rightSubtree) =
  let (minLeft, newLeftSubtree) = extractMin leftSubtree
  in (minLeft, Branch value newLeftSubtree rightSubtree)