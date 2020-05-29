{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GADTs                    #-}
module LuaSolve where

import Solve

import qualified Data.HashMap.Strict   as M
import           Data.Text             (Text)
import           Foreign.Lua
import           Numeric.LinearAlgebra (Field)

solveFactory :: (Field a, Key k, Key l, Ord a)
  => [(k,a)] -> [k] -> [(k,[(l,a)])] -> [[(l,a)]]
solveFactory fixed free m = M.toList <$> smartSolve fixed' free m'
  where
    fixed' = M.fromList fixed
    m' = fmap M.fromList . M.fromList $ m

luaSolve' :: (a ~ Double, k ~ Text, l ~ Text)
  => [(k,a)] -> [k] -> [(k,[(l,a)])] -> Lua [[(l,a)]]
luaSolve' fixed free = pure . solveFactory fixed free

foreign export ccall luaSolve :: State -> IO ()

luaSolve :: State -> IO ()
luaSolve s = runWith s $ do
  fixed <- peek 1
  free <- peek 2
  matrix <- peek 3
  out <- luaSolve' fixed free matrix
  push out

