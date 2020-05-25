{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
module Recipes where

import Solver

import           Control.Monad         (join)
import           Data.Hashable         (Hashable)
import           Data.HashMap.Strict   (HashMap)
import qualified Data.HashMap.Strict   as M
import           Data.List             (elemIndex, nub, sort, subsequences)
import           Data.List.Extra       (disjoint)
import           Data.List.Ordered     (subset)
import           Data.Maybe            (isNothing, mapMaybe)
import           Data.String           (IsString)
import           Data.Text             (Text)
import qualified Data.Text             as T
import           Data.Tuple            (swap)
import           Numeric.LinearAlgebra

newtype Item = Item Text deriving (Eq, Hashable, IsString, Ord, Show)
newtype Name = Name Text deriving (Eq, Hashable, IsString, Ord, Show)
type Recipe a = (Name, HashMap Item a)
type Cookbook a = HashMap Name (HashMap Item a)
type Constraint a = HashMap Item (Maybe a)

solveFactory :: Field a => Cookbook a -> Constraint a -> [HashMap Name a]
solveFactory b c = solve fixed free (trans b)
  where
    fixed = M.mapMaybe id c
    free = M.keys . M.filter isNothing $ c

-- For testing

test = trans angIron
fixed = M.mapMaybe id ironOut
free = M.keys . M.filter isNothing $ ironOut

-- Simple example
mkIron :: Recipe Double
mkIron = ("Make iron", [("iron", 1)])

mkSteel :: Recipe Double
mkSteel = ("Make steel", [("steel", 1), ("iron", -5)])

simple :: Cookbook Double
simple = M.fromList [ mkIron, mkSteel ]

-- planFactory simple simple1 should succeed
simple1 :: Constraint Double
simple1 = [("steel", Just 2)]

-- planFactory simple simple2 should fail
simple2 :: Constraint Double
simple2 = [("steel", Nothing)]

-- Complex example
saphCrush :: Recipe Double
saphCrush = ("Saphirite crushing",)
  [ ("saphirite ore", -2)
  , ("crushed saphirite", 2)
  , ("crushed stone", 1) ]

saphRef :: Recipe Double
saphRef = ("Saphirite refining",)
  [ ("crushed saphirite", -2)
  , ("purified water", -50)
  , ("saphirite chunk", 2)
  , ("blue geode", 0.5)
  , ("sulfuric water", 50) ]

chunkSort :: Recipe Double
chunkSort = ("Saphirite chunk sorting",)
  [ ("saphirite chunk", -6)
  , ("slag", 1)
  , ("copper ore", 1)
  , ("silicon ore", 1)
  , ("nickle ore", 1)
  , ("iron ore", 2) ]

geoCrush :: Recipe Double
geoCrush = ("Geode crushing",)
  [ ("blue geode", -2)
  , ("crystal dust", 1)
  , ("crushed stone", 2) ]

sulfPure :: Recipe Double
sulfPure = ("Sulfuric water purification",)
  [ ("sulfuric water", -100)
  , ("sulfur", 1)
  , ("mineralized water", 20)
  , ("purified water", 70) ]

sulfDiox :: Recipe Double
sulfDiox = ("Sulfuric dioxide",)
  [ ("sulfur", -1)
  , ("oxygen", -60)
  , ("sulfur dioxide", 60) ]

sulfAcid :: Recipe Double
sulfAcid = ("Sulfuric acid",)
  [ ("sulfur dioxide", -90)
  , ("purified water", -40)
  , ("sulfuric acid", 60) ]

slagSlur :: Recipe Double
slagSlur = ("Slag slurry from slag",)
  [ ("slag", -5)
  , ("sulfuric acid", -15)
  , ("slag slurry", 50) ]

slagSlur2 :: Recipe Double
slagSlur2 = ("Slag slurry from stone",)
  [ ("crushed stone", -25)
  , ("sulfuric acid", -15)
  , ("slag slurry", 50) ]

slurCoal :: Recipe Double
slurCoal = ("Slurry coal filtering",)
  [ ("slag slurry", -50)
  , ("coal filter", -1)
  , ("purified water", -50)
  , ("mineral sludge", 50)
  , ("sulfuric water", 40)
  , ("filter frame", 1) ]

oxy :: Recipe Double
oxy = ("Oxygen",)
  [ ("compressed air", -100)
  , ("oxygen", 50)
  , ("nitrogen", 50) ]

coalFilt :: Recipe Double
coalFilt = ("Coal filters",)
  [ ("filter frame", -5)
  , ("coal", -1)
  , ("coal filter", 5) ]

pureWater :: Recipe Double
pureWater = ("Purified water",)
  [ ("water", -150)
  , ("purified water", 100)
  , ("saline water", 20) ]

-- Rank 13 as a matrix, so can specify up to 13 wanted levels
-- and (num items - 13) free levels.
angSaph :: Cookbook Double
angSaph = M.fromList
  [ saphCrush
  , saphRef
  , chunkSort
  , geoCrush
  , sulfPure
  , sulfDiox
  , sulfAcid
  , slagSlur
  , slagSlur2
  , slurCoal
  , oxy
  , coalFilt
  , pureWater ]

someOreOut :: Int -> Constraint Double
someOreOut n = M.fromList $ take n
  [ ("coal", Nothing)
  , ("compressed air", Nothing)
  , ("copper ore", Nothing)
  , ("crystal dust", Nothing)
  , ("iron ore", Just 15)
  , ("mineral sludge", Nothing)
  , ("mineralized water", Nothing)
  , ("nickle ore", Nothing)
  , ("nitrogen", Nothing)
  , ("saline water", Nothing)
  , ("silicon ore", Nothing)
  , ("sulfuric acid", Nothing)
  , ("sulfuric water", Nothing)
  , ("water", Nothing) ]

oreOut :: Constraint Double
oreOut =
  [ ("coal", Nothing)
  , ("compressed air", Nothing)
  , ("copper ore", Nothing)
  , ("crystal dust", Nothing)
  , ("iron ore", Just 15)
  , ("mineral sludge", Nothing)
  , ("mineralized water", Nothing)
  , ("nickle ore", Nothing)
  , ("nitrogen", Nothing)
  , ("saline water", Nothing)
  , ("silicon ore", Nothing)
  , ("sulfuric acid", Nothing) ]

-----

procIron :: Recipe Double
procIron = ("Process iron",)
  [ ("iron ore", -4)
  , ("processed iron", 2) ]

ironIngot :: Recipe Double
ironIngot = ("Iron ingot",)
  [ ("processed iron", -8)
  , ("coke", -2)
  , ("iron ingot", 24) ]

moltenIron :: Recipe Double
moltenIron = ("Molten iron",)
  [ ("iron ingot", -12)
  , ("molten iron", 120) ]

ironPlate :: Recipe Double
ironPlate = ("Iron plate",)
  [ ("molten iron", -40)
  , ("iron plate", 4) ]

angIron :: Cookbook Double
angIron = [procIron, ironIngot, moltenIron, ironPlate]

ironOut :: Constraint Double
ironOut = [ ("iron ore", Just (-7.5)), ("iron plate", Nothing) ]

