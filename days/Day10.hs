module Main where

import Paths_aoc2020
import Data.List hiding (permutations)

diffs :: [Int] -> [Int]
diffs nums = zipWith (-) (sort nums <> [maximum nums + 3]) (0 : sort nums)

onesTimesThrees :: [Int] -> Int
onesTimesThrees xs = 
  let
    ds = diffs xs
    ones = length (filter (== 1) ds)
    threes = length (filter (== 3) ds)
  in ones * threes

data Bit = One | Zero
  deriving (Show, Eq)

permutations :: Int -> [[Bit]]
permutations 0 = [[]]
permutations n =
  let ps = permutations (n-1)
  in [ Zero : x | x <- ps ] <> [ One : x | x <- ps ]

allowed :: [Bit] -> Bool
allowed = null . filter (\g -> elem Zero g && length g >= 3) . group

allowedPerms :: Int -> Int
allowedPerms = length . filter allowed . permutations

arrangements :: [Int] -> Int
arrangements =
  product . map (allowedPerms . pred . length) . filter (elem 1) . group . diffs

main :: IO ()
main = do
  nums <- fmap (map read . lines) . readFile =<< getDataFileName "inputs/day10"
  print (onesTimesThrees nums)
  print (arrangements nums)
