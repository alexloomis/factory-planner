module Solver where

import           Control.Monad         (join)
import           Data.Hashable         (Hashable)
import           Data.HashMap.Strict   (HashMap)
import qualified Data.HashMap.Strict   as M
import           Data.List             (elemIndex, nub, sort, subsequences,
                                        (\\))
import           Data.List.Extra       (disjoint, groupSortOn)
import           Data.List.Ordered     (subset)
import           Data.Maybe            (catMaybes, mapMaybe)
import           Data.Text             (Text)
import qualified Data.Text             as T
import           Data.Tuple            (swap)
import           Numeric.LinearAlgebra

type DoubleMap k l a = HashMap k (HashMap l a)
type Key k = (Hashable k, Ord k)

pullMaybe :: Monad m => (m a, m b) -> m (a, b)
pullMaybe = (>>= (fmap swap . sequence . swap)) . sequence

innerKeys :: Eq l => DoubleMap k l a -> [l]
innerKeys = nub . concatMap M.keys . M.elems

keyToInt :: Ord k => k -> DoubleMap k l a -> Maybe Int
keyToInt n = elemIndex n . sort . M.keys

innerKeyToInt :: Ord l => l -> DoubleMap k l a -> Maybe Int
innerKeyToInt n = elemIndex n . sort . innerKeys

toAssocList :: DoubleMap k l a -> [((k, l), a)]
toAssocList = fmap f . concatMap sequence . M.toList . fmap M.toList
  where f (n, (i, a)) = ((n, i), a)

toAssocInt :: (Ord k, Ord l) =>  DoubleMap k l a -> [((Int, Int), a)]
toAssocInt b = mapMaybe f . toAssocList $ b
  where
    f ((n, i), a) = pullMaybe (pullMaybe (keyToInt n b, innerKeyToInt i b), Just a)

fromAssocList :: (Key k, Key l) => [((k, l), a)] -> DoubleMap k l a
fromAssocList = fmap M.fromList . M.fromList . fmap shift . groupSortOn fst . fmap f
  where
    shift xs = (fst . head $ xs, fmap snd xs)
    f ((k, l), a) = (k, (l, a))

trans :: (Key k, Key l) => DoubleMap k l a -> DoubleMap l k a
trans = fromAssocList . fmap f . toAssocList
  where f ((l, k), a) = ((k, l), a)

-- Outer keys are rows.
toMatrix :: (Container Vector a, Num a, Ord k, Ord l)
  => DoubleMap k l a -> Matrix a
toMatrix b = assoc (M.size b, length $ innerKeys b) 0 (toAssocInt b)

toMatrix' :: (Container Vector a, Num a, Ord k) => HashMap k a -> Matrix a
toMatrix' = toMatrix . fmap (M.fromList . pure . ((),))

-- Are the rows independent?
isInd :: (Field a, Ord k, Ord l) => DoubleMap k l a -> Bool
isInd m = rank (toMatrix m) == M.size m

-- Choices of n elements from a list.
choices :: Int -> [a] -> [[a]]
choices n = filter (\x -> length x == n) . subsequences

-- SubMaps with n keys.
allSizeNSub :: Key k => Int -> HashMap k v -> [HashMap k v]
allSizeNSub n m = fmap (foldr M.delete m)
  . choices (M.size m - n) . M.keys $ m

-- SubMaps with n keys, excluding some.
allSizeNSubBut :: Key k => Int -> [k] -> HashMap k v -> [HashMap k v]
allSizeNSubBut n free m =
  [ M.filterWithKey (\k v -> k `elem` keyList) m
  | keyList <- choices n . (\\ free) . M.keys $ m ]

-- The max rank submatricies with independent rows.
allMaxInd :: (Field a, Key k, Ord l)
  => DoubleMap k l a -> [DoubleMap k l a]
allMaxInd m = filter isInd . allSizeNSub (rank $ toMatrix m) $ m

-- The max rank submatricies with independent rows, excluding some.
allMaxIndBut :: (Field a, Key k, Ord l)
  => [k] -> DoubleMap k l a -> [DoubleMap k l a]
allMaxIndBut free m = filter isInd . allSizeNSubBut (rank $ toMatrix m) free $ m

-- The max rank submatricies with independent rows,
-- including or excluding certain rows.
chooseInd :: (Field a, Key k, Ord l)
  => [k] -> [k] -> DoubleMap k l a -> [DoubleMap k l a]
chooseInd fixedKeys free = filter hasKeys . allMaxIndBut free
  where hasKeys b = null $ fixedKeys \\ M.keys b

chooseCols :: (Field a, Key k, Key l) => DoubleMap k l a -> [DoubleMap k l a]
chooseCols = fmap trans . allMaxInd . trans

-- All submatricies with independent rows and columns, respecting row preferences.
chooseBoth :: (Field a, Key k, Key l)
  => [k] -> [k] -> DoubleMap k l a -> [DoubleMap k l a]
chooseBoth fixedKeys free m = concatMap chooseCols (chooseInd fixedKeys free m)

-- All legal constraints where extra terms to zero.
allExtraZero :: (Field a, Key k, Num v, Ord l)
  => HashMap k v -> [k] -> DoubleMap k l a -> [HashMap k v]
allExtraZero cs free = fmap (M.union cs . fmap (const 0))
  . chooseInd (M.keys cs) free

-- Solve Ax = B, where B is a column matrix.
innerSolver :: (Field a, Key l, Ord k)
  => DoubleMap k l a -> HashMap k a -> Maybe (HashMap l a)
innerSolver b c
  | M.keys b == M.keys c = fromCol <$> linearSolve (toMatrix b) (toMatrix' c)
  | otherwise = Nothing
  where
    fromCol = M.fromList . zip (sort . innerKeys $ b) . concat . toLists

-- Takes a list of fixed values, a list of free values,
-- and a DoubleMap, and attempts to solve.
solve :: (Field a, Key k, Key l)
  => HashMap k a -> [k] -> DoubleMap k l a -> [HashMap l a]
solve fixed free m = catMaybes
  [ innerSolver x y
  | x <- chooseBoth (M.keys fixed) free m
  , y <- allExtraZero fixed free m ]
