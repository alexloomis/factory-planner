{-# LANGUAGE GADTs #-}
module Main where

import Solve

import qualified Data.HashMap.Strict   as M
import           Data.Text             (Text)
import           Foreign.Lua           (Lua)
import qualified Foreign.Lua           as Lua
import           Numeric.LinearAlgebra (Field)

solveFactory :: (Field a, Key k, Key l, Ord a)
  => [(k,a)] -> [k] -> [(k,[(l,a)])] -> [[(l,a)]]
solveFactory fixed free m = M.toList <$> smartSolve fixed' free m'
  where
    fixed' = M.fromList fixed
    m' = fmap M.fromList . M.fromList $ m

luaSolve :: (a ~ Double, k ~ Text, l ~ Text)
  => [(k,a)] -> [k] -> [(k,[(l,a)])] -> Lua [[(l,a)]]
luaSolve fixed free = pure . solveFactory fixed free

main :: IO ()
main = Lua.run $
  Lua.registerHaskellFunction "solve_recipe_matrix" luaSolve

