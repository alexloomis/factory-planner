{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GADTs                    #-}

module LuaSolve where

import Solve

import qualified Control.Monad.Catch    as Catch
import           Control.Monad.IO.Class (MonadIO)
import qualified Data.HashMap.Strict    as M
import           Data.Map.Strict        (fromList)
import           Data.Text              (Text)
import           Foreign.Lua.Core
import           Foreign.Lua.Peek
import           Foreign.Lua.Push
import           Numeric.LinearAlgebra  (Field)
-- import           Type.Reflection        (Typeable, typeOf)

pushKeyValueList kp vp kv = pushMap kp vp (fromList kv)

solveFactory :: (Field a, Key k, Key l, Ord a)
  => [(k,a)] -> [k] -> [(k,[(l,a)])] -> [[(l,a)]]
solveFactory fixed free m = M.toList <$> smartSolve fixed' free m'
  where
    fixed' = M.fromList fixed
    m' = fmap M.fromList . M.fromList $ m

luaSolve' :: (a ~ Double, k ~ Text, l ~ Text)
  => [(k,a)] -> [k] -> [(k,[(l,a)])] -> [[(l,a)]]
luaSolve' = solveFactory

foreign export ccall luaSolve :: State -> IO Int

luaSolve :: State -> IO Int
luaSolve s = runWithConverter unsafeErrorConversion s $ do
  a <- peekKeyValuePairs peekText peekRealFloat 1
  b <- peekList peekText 2
  c <- peekKeyValuePairs peekText (peekKeyValuePairs peekText peekRealFloat) 3
  case (a, b, c) of
    (Right fixed, Right free, Right matrix)
      -> pushList (pushKeyValueList pushText pushRealFloat) $ luaSolve' fixed free matrix
    _ -> pushString $ show a ++ show b ++ show c
  return 1
  {-
  where
    printV :: (Show a, MonadIO m) => a -> m ()
    printV = liftIO . print
    printT :: (Typeable a, MonadIO m) => a -> m ()
    printT = liftIO . print . typeOf
-}

