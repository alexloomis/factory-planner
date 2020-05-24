{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections    #-}
module Main where

import Control.Monad         (join)
import Data.List             (intersect, sort, subsequences)
import Data.Maybe            (catMaybes, mapMaybe)
import Numeric.LinearAlgebra

data Error

main :: IO ()
main = undefined

testMatrix :: Matrix Double
testMatrix = (4 >< 5) $ [4,2,4,6,8,2] ++ [1..]

-- Are the rows independent?
isInd :: Field a => Matrix a -> Bool
isInd m = rank m == rows m

-- All increasing sequences of n nonnegative integers bounded by b.
choices :: Int -> Int -> [[Int]]
choices n b = filter (\x -> length x == n) $ subsequences [0..b]

-- Submatricies with n rows, along with their indicies.
allSizeNSub :: Element a => Int -> Matrix a -> [([Int], Matrix a)]
allSizeNSub n m = [ (idx, m ? idx) | idx <- choices n (rows m - 1)]

-- The max rank submatricies with independent rows.
allMaxInd :: Field a => Matrix a -> [([Int], Matrix a)]
allMaxInd m = filter (isInd . snd) $ allSizeNSub (rank m) m

-- The max rank submatricies with independent rows,
-- including or excluding certain rows.
-- Note that intersect preserves the order of the first argument.
chooseInd :: Field a
  => [Int] -> [Int] -> Matrix a -> [([Int], Matrix a)]
chooseInd keep toss = filter p . allMaxInd
  where
    p (xs,_) = keep `intersect` xs == keep && null (toss `intersect` xs)

-- All submatricies with independent rows and columns,
-- respecting row preferences.
-- Output format is (rows, cols, matrix).
chooseBoth :: Field a
  => [Int] -> [Int] -> Matrix a -> [([Int], [Int], Matrix a)]
chooseBoth keep toss m = concatMap chooseCols (chooseInd keep toss m)
  where
    chooseCols (xs, m') = flatten (xs, fmap (fmap tr) . allMaxInd . tr $ m')
    flatten (xs, ms) = [ (xs, fst ms', snd ms') | ms' <- ms ]

-- All legal constraints where extra terms to zero.
allExtraZero :: Field a => [(Int, a)] -> [Int] -> Matrix a -> [[(Int, a)]]
allExtraZero cs toss = fmap (f . fmap (,0)) . ids
  where
    ids = fmap fst . chooseInd (fmap fst cs) toss
    f xs = cs ++ filter (\(x,_) -> x `notElem` fmap fst cs) xs

-- Attaches values from the first list to keys from the second.
-- Careful, does not fail when second list strict subset of first.
match :: (Traversable t, Eq a) => [(a, b)] -> t a -> Maybe (t b)
match = mapM . flip lookup

-- Solve Ax = B, ensuring the corresponding rows of B are in the right order.
innerSolver :: Field a => ([Int], b, Matrix a) -> [(Int, a)] -> Maybe (Matrix a)
innerSolver (rows, _, mat) = (>>= f) . flip match rows
  where f xs = linearSolve mat $ (length xs >< 1) xs

solver :: Field a
  => [(Int, a)] -> [Int] -> Matrix a -> [([Int], Matrix a)]
solver fixed free mat = mapMaybe sequence
  [ ((\(_,c,_) -> c) x, innerSolver x y)
  | x <- chooseBoth (fmap fst fixed) free mat
  , y <- allExtraZero fixed free mat ]

-- Takes a list of constraints and a matrix and attempts to solve.
-- Constraints of the form (n, Nothing) do not attempt to force item n
-- to have net zero production.
-- Constraints of the form (n, Just m) force item n to have an output of m,
-- or equivalently, forces consumption of -m of item n.
solve :: Field a => [(Int, Maybe a)] -> Matrix a -> [([Int], Matrix a)]
solve cs = uncurry solver (separate cs)
  where
    separate xs =
      ( [ (a,b) | (a, Just b) <- xs ]
      , [ a | (a, Nothing) <- xs ] )

