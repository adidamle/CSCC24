module Quadtree where

import Data.Array
import Data.Word

import QuadtreeDef

---------------------------------------------------- QUESTION 1 -------------------------------------------------------------------------
-- HELPER FUNCTION FOR THE QUADTREETOPIC.
-- This is the base case when the Qnode has NO or 0 children i.e. when QKids is Q0.
-- For the base case we have to fill in a sub matrix with the 'g' or the grayscale value. To do this task I made use of the
-- list comprehension to mimic the for loop which I needed to make all the co-ordinates or the indexes.
-- the list consists of tuples as follows ((x-cordinate, y-cordinate), grayscale value)
-- for the recuursion case, we have to recurse on each of the 4 children and append it to a list.
quadtreeToPicListImplementation :: Quadtree -> [((Int, Int), Word8)]
quadtreeToPicListImplementation (QNode x y w g Q0) = [((x + i, y + j), g) | i <- [0..w-1], j <- [0..w-1]]
quadtreeToPicListImplementation (QNode x y w g (Q4 child1 child2 child3 child4)) = quadtreeToPicListImplementation child1 ++
                                                                   quadtreeToPicListImplementation child2 ++
                                                                   quadtreeToPicListImplementation child3 ++
                                                                   quadtreeToPicListImplementation child4


quadtreeToPic :: Quadtree -> Array (Int, Int) Word8
-- Make array of the desired dimensions using the given cordinates and the width.
-- To fill in the 2D array, call in the helper function.
quadtreeToPic (QNode x y w g children) = array ((x, y), (x+w-1, y+w-1)) (quadtreeToPicListImplementation (QNode x y w g children))


---------------------------------------------------- QUESTION 2 -------------------------------------------------------------------------

-- This function finds the grayscale difference of the inputed array (matrix)
-- grayscale difference = (max item of the matrix) - (min item of the matrix)
calculateGrayscaleDifference :: Array (Int, Int) Word8 -> Word8
calculateGrayscaleDifference arr = 
    let lst = elems arr
    in (maximum lst) - (minimum lst)


-- This function finds the grayscale difference of the inputed array (matrix)
-- grayscale difference = (max item of the matrix) - (min item of the matrix)
calculateGrayscaleAvgValue :: Array (Int, Int) Word8 -> Word8
calculateGrayscaleAvgValue arr = 
    let a = map (fromIntegral) (elems arr)
        s = fromIntegral (sum a)
        l = fromIntegral (length a)
    in round (s / l)


-- This function calculates the width of the given 2D array.
calculateWidth :: Array (Int, Int) Word8 -> Int
calculateWidth arr = 
    let b = bounds arr
    in (fst (snd b)) - (fst (fst b)) + 1


-- This function takes in the array to split, x, y, width as the paramenter to it
-- and returns a new array whose indices are 
splitArray :: Array (Int, Int) Word8 -> Int -> Int -> Int -> Array (Int, Int) Word8
splitArray arr x y w = 
    let lst = filter (\item -> inRange ((x, y), (x+w-1, y+w-1)) (fst item)) (assocs arr)
    in array ((x, y), (x+w-1, y+w-1)) lst


----------------------- MAIN QUESTION ---------------------------------------

picToQuadtree :: Word8                    -- threshold
              -> Int                      -- depth cap
              -> Array (Int, Int) Word8   -- image
              -> Quadtree

-- We have 4 base cases which all return a quadtree with no children i.e. Qnode x y w avg Q0
-- 1. When the depth cap get 0.
-- 2. When the threshold is too high i.e. 255 then all the differences are less than 255 so we want to return.
-- 3. When the width is 0 i.e.we have hit a pixel and we cant split more, then we have to stop and return.
-- 4. We also want to stop when the grayscale difference gets below threshold.
-- For the recurrsion case we recurse on 4 chidren which follow the order top left, top bottom, top right, bottom right.
picToQuadtree threshold depthCap arr
    | depthCap == 0 = simpleQuadTree
    | threshold == 255 = simpleQuadTree
    | w == 0 = simpleQuadTree
    | gsDifference <= threshold = simpleQuadTree
    | otherwise = QNode x y w avg (Q4 c1 c2 c3 c4)
    where x = (fst (fst (bounds arr))) 
          y = (snd (fst (bounds arr)))
          w = calculateWidth arr
          avg = calculateGrayscaleAvgValue arr
          gsDifference = calculateGrayscaleDifference arr
          simpleQuadTree = QNode x y w avg Q0
          c1 = picToQuadtree threshold (depthCap-1) (splitArray arr x y (div w 2))
          c2 = picToQuadtree threshold (depthCap-1) (splitArray arr x (y + (div w 2)) (div w 2))
          c3 = picToQuadtree threshold (depthCap-1) (splitArray arr (x + (div w 2)) y (div w 2))
          c4 = picToQuadtree threshold (depthCap-1) (splitArray arr (x + (div w 2)) (y + (div w 2)) (div w 2))