module Coursework where

{-
  Your task is to design a datatype that represents the mathematical concept of a (finite) set of elements (of the same type).
  We have provided you with an interface (do not change this!) but you will need to design the datatype and also 
  support the required functions over sets.
  Any functions you write should maintain the following invariant: no duplication of set elements.

  There are lots of different ways to implement a set. The easiest is to use a list
  (as in the example below). Alternatively, one could use an algebraic data type, or 
  wrap a binary search tree.
  Extra marks will be awarded for efficient implementations if appropriate.

  You are NOT allowed to import anything from the standard library or other libraries.
  Your edit of this file should be completely self-contained.

  DO NOT change the type signatures of the functions below: if you do,
  we will not be able to test them and you will get 0% for that part. While sets are unordered collections,
  we have included the Ord constraint on most signatures: this is to make testing easier.

  You may write as many auxiliary functions as you need. Please include everything in this file.
-}

{-
   PART 1.
   You need to define a Set datatype. Below is an example which uses lists internally.
   It is here as a guide, but also to stop ghci complaining when you load the file.
   Free free to change it.
-}

-- you may change this to your own data type
-- newtype Set a = Set { unSet :: [a] }
data Set a = Null | Node (Set a) a (Set a) deriving Show

{-
   PART 2.
   If you do nothing else, at least get the following two functions working. They
   are required for testing purposes.
-}

-- toList {2,1,4,3} => [1,2,3,4]
-- the output must be sorted.
toList :: Set a -> [a]
toList Null = []
toList (Node s1 n s2) = toList s1 ++ [n] ++ toList s2

-- fromList [2,1,1,4,5] => {2,1,4,5}
fromList :: Ord a => [a] -> Set a
fromList = foldr insert Null

{-
   PART 3.
   Your Set should contain the following functions.
   DO NOT CHANGE THE TYPE SIGNATURES.
-}

-- test if two sets have the same elements.
instance (Ord a) => Eq (Set a) where
  s1 == s2 = and $ [x `member` s2 | x <- toList s1] ++ [x `member` s1 | x <- toList s2]

-- the empty set
empty :: Set a
empty = Null


-- Set with one element
singleton :: a -> Set a
singleton x = Node Null x Null


-- insert an element of type a into a Set
-- make sure there are no duplicates!
insert :: (Ord a) => a -> Set a -> Set a
insert x Null = Node Null x Null
insert x (Node s1 n s2)
   | x == n = Node s1 n s2
   | x < n  = Node (insert x s1) n s2 
   | x > n  = Node s1 n (insert x s2)


-- join two Sets together
-- be careful not to introduce duplicates.
union :: (Ord a) => Set a -> Set a -> Set a
union s1 s2 = fromList (toList s1 ++ toList s2)


-- return the common elements between two Sets
intersection :: (Ord a) => Set a -> Set a -> Set a
intersection s1 s2 = fromList (intersection' (toList s1) (toList s2))
   where
      intersection' :: (Ord a) => [a] -> [a] -> [a]
      intersection' xs ys = [a | a <- xs, a `elem` ys]


-- all the elements in Set A *not* in Set B,
-- {1,2,3,4} `difference` {3,4} => {1,2}
-- {} `difference` {0} => {}
difference :: (Ord a) => Set a -> Set a -> Set a
difference s1 s2 = fromList (difference' (toList s1) (toList s2))
   where
      difference' :: (Ord a) => [a] -> [a] -> [a]
      difference' xs ys = [x | x <- xs, x `notElem` ys]


-- is element *a* in the Set?
member :: (Ord a) => a -> Set a -> Bool
member x s = x `elem` toList s


-- how many elements are there in the Set?
cardinality :: Set a -> Int
cardinality s = length $ toList s


setmap :: (Ord b) => (a -> b) -> Set a -> Set b
setmap f s = fromList $ map f $ toList s


setfoldr :: (a -> b -> b) -> Set a -> b -> b
setfoldr f s x = foldr f x $ toList s


-- powerset of a set
-- powerset {1,2} => { {}, {1}, {2}, {1,2} }
powerSet :: Set a -> Set (Set a)
powerSet Null = Node Null Null Null
-- powerSet (Node s1 x s2) = undefined
-- powerSet s = undefined
powerSet s = fromList' $ powerSet' (toList s)
   where
      powerSet' :: [a] -> [Set a]
      powerSet' [] = [Null]
      powerSet' (x:xs) = map (insert' x) (powerSet' xs) ++ powerSet' xs

-- cartesian product of two sets
cartesian :: Set a -> Set b -> Set (a, b)
cartesian s1 s2 = fromList' [(x,y) | x <- toList s1, y <- toList s2]

-- auxiliary functions
-- bad time complexity but works
fromList' :: [a] -> Set a
fromList' = foldr insert' Null

insert' :: a -> Set a -> Set a
insert' x Null = Node Null x Null
insert' x (Node s1 n s2) = Node (insert' x s1) n s2

toList' :: Set (Set a) -> [[a]]
toList' ss = [toList s | s <- toList ss]


-- partition the set into two sets, with
-- all elements that satisfy the predicate on the left,
-- and the rest on the right
partition :: (a -> Bool) -> Set a -> (Set a, Set a)
partition p s = (fromList' [x | x <- toList s, p x], fromList' [x | x <- toList s, not (p x)])

{-
   On Marking:
   Be careful! This coursework will be marked using QuickCheck, against Haskell's own
   Data.Set implementation. Each function will be tested for multiple properties.
   Even one failing test means 0 marks for that function.

   Marks will be lost for too much similarity to the Data.Set implementation.

   Pass: creating the Set type and implementing toList and fromList is enough for a
   passing mark of 40%.

-}
