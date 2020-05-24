{-# LANGUAGE OverloadedStrings #-}
module Recipes where

import           Control.Monad         (join)
import           Data.Hashable         (Hashable)
import           Data.HashMap.Strict   (HashMap)
import qualified Data.HashMap.Strict   as M
import           Data.List             (elemIndex, nub, sort, subsequences)
import           Data.List.Extra       (disjoint)
import           Data.List.Ordered     (subset)
import           Data.Maybe            (mapMaybe)
import           Data.Text             (Text)
import qualified Data.Text             as T
import           Data.Tuple            (swap)
import           Numeric.LinearAlgebra

newtype Item = Item Text deriving (Eq, Hashable, Ord, Show)
newtype Name = Name Text deriving (Eq, Hashable, Ord, Show)
type Recipe a = HashMap Item a
type Cookbook a = HashMap Name (Recipe a)

-- Examples for testing
mkIron :: Recipe Int
mkIron = M.fromList [(Item "Iron",1)]

mkSteel :: Recipe Int
mkSteel = M.fromList [(Item "Steel",1), (Item "Iron",-5)] :: Recipe Int

book :: Cookbook Int
book = M.fromList [(Name "Make Iron", mkIron), (Name "Make Steel", mkSteel)]

