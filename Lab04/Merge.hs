module Merge where

-- There are other questions in other files.

{-
[4 marks]
Take two lists of integers.  Precondition: Each is already sorted (non-decreasing).
Perform the "merge" of "mergesort".  Linear time.

Example:
merge [2, 3, 5] [1, 3, 4] = [1, 2, 3, 3, 4, 5]
-}
merge :: [Integer] -> [Integer] -> [Integer]
merge [] lst2 = lst2
merge lst1 [] = lst1
merge lst1@(x:xs) lst2@(y:ys)
      | x <= y = x : (merge xs (y:ys))
      | otherwise = y: (merge ys (x:xs))
