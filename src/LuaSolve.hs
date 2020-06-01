{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GADTs                    #-}

module LuaSolve where

import Solve

import qualified Control.Monad.Catch    as Catch
import           Control.Monad.IO.Class (MonadIO)
import qualified Data.HashMap.Strict    as M
import           Data.Text              (Text)
import           Foreign.Lua
import           Numeric.LinearAlgebra  (Field)
import           Type.Reflection        (Typeable, typeOf)

solveFactory :: (Field a, Key k, Key l, Ord a)
  => [(k,a)] -> [k] -> [(k,[(l,a)])] -> [[(l,a)]]
solveFactory fixed free m = M.toList <$> smartSolve fixed' free m'
  where
    fixed' = M.fromList fixed
    m' = fmap M.fromList . M.fromList $ m

luaSolve' :: (a ~ Double, k ~ Text, l ~ Text)
  => [(k,a)] -> [k] -> [(k,[(l,a)])] -> [[(l,a)]]
luaSolve' = solveFactory

-- | Use @test@ to check whether the value at stack index @n@ has the correct
-- type and use @peekfn@ to convert it to a haskell value if possible. A
-- successfully received value is wrapped using the @'Success'@ constructor,
-- while a type mismatch results in an @Error@ with the given error message.
typeChecked :: String
            -> (StackIndex -> Lua Bool)
            -> (StackIndex -> Lua a)
            -> StackIndex -> Lua a
typeChecked = const (const id)

-- | Specify a name for the context in which a computation is run. The name is
-- added to the error message in case of an exception.
inContext :: String -> Lua a -> Lua a
inContext = const id

-- | Get the next key-value pair from a table. Assumes the last key to be on the
-- top of the stack and the table at the given index @idx@.
nextNestedPair :: (Peekable a, Peekable b, Peekable c)
  => StackIndex -> Lua (Maybe (a, [(b, c)]))
nextNestedPair idx = do
  hasNext <- next idx
  if hasNext
    then let pair = (,) <$> inContext "Could not read key of key-value pair: "
                                      (peek (nthFromTop 2))
                        <*> inContext "Could not read value of key-value pair: "
                                      (peekKeyValuePairs (nthFromTop 1))
         in Just <$> pair `Catch.finally` pop 1
            -- removes the value, keeps the key
    else return Nothing

-- | Read a table into a list of pairs.
peekNestedKeyValuePairs :: (Peekable a, Peekable b, Peekable c)
  => StackIndex -> Lua [(a, [(b, c)])]
peekNestedKeyValuePairs = typeChecked "table" istable $ \idx -> do
  let remainingPairs = do
        res <- nextNestedPair (if idx < 0 then idx - 1 else idx)
        case res of
          Nothing -> [] <$ return ()
          Just a  -> (a:) <$> remainingPairs
  pushnil
  remainingPairs
    -- ensure the remaining key is removed from the stack on exception
    `Catch.onException` pop 1

foreign export ccall luaSolve :: State -> IO Int

luaSolve :: State -> IO Int
luaSolve s = runWith s $ do
  fixed <- peekKeyValuePairs 1 :: Lua [(Text, Double)]
  free <- peekList 2 :: Lua [Text]
  matrix <- peekNestedKeyValuePairs 3 :: Lua [(Text, [(Text, Double)])]
  push $ luaSolve' fixed free matrix
  return 1
  -- where
    -- printV :: (Show a, MonadIO m) => a -> m ()
    -- printV = liftIO . print
    -- printT :: (Typeable a, MonadIO m) => a -> m ()
    -- printT = liftIO . print . typeOf

