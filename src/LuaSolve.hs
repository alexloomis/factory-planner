{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GADTs                    #-}

module LuaSolve where

import Solve

import Data.Map.Strict (Map)
import Data.Text       (Text)
import Foreign.Lua

luaSolve' :: (a ~ Double, k ~ Text, l ~ Text)
  => Map k a -> [k] -> DoubleMap k l a -> [Map l a]
luaSolve' = smartSolve

foreign export ccall luaSolve :: State -> IO Int

luaSolve :: State -> IO Int
luaSolve s = runWithConverter unsafeErrorConversion s $ do
  a <- peek 1
  b <- peek 2
  c <- peek 3
  push $ luaSolve' a b c
  return 1

