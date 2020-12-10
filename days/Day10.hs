module Main where

import Paths_aoc2020
import Data.List hiding (permutations)

-- We need to find the differences between all the adapters, in sorted order. To do
-- this we are going to use a zipper between two lists:
--
-- * 0 appended to the sorted adapters (the socket starts at zero).
-- * the sorted adapters with the phone value added at the end (the maximum jolt
-- value from the adapters plus 3)
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

-- This function finds all the possible combinations of lists of bits of length
-- n. So for example:
--
-- permutations 2
--
-- >> [[Zero,Zero],[Zero,One],[One,Zero],[One,One]]
--
permutations :: Int -> [[Bit]]
permutations 0 = [[]]
permutations n =
  let ps = permutations (n-1)
  in [ Zero : x | x <- ps ] <> [ One : x | x <- ps ]

-- Check if a particular sequence of adapters is allowed. We need to check that
-- there isn't a distance of more than 3 between any adapter in the chain. We
-- also know that since this is a group of 1s, every adapter is one away from
-- it's adjacent adapters.
--
-- The sequence of bits represents if we keep the adapter or not in the current
-- permutation. If there is a sequence of Zeros that is >= 3, then that means
-- that there will be a joltage gap of more than three, which is not allowed.
--
-- So, we:
--
-- 1. Split the list of bits into groups of Zeros and Ones
-- 2. Keep only the groups that are all Zeros and also are of length >= 3
-- 3. Check that the filtered list is empty
--
allowed :: [Bit] -> Bool
allowed = null . filter (\g -> elem Zero g && length g >= 3) . group

-- For a group of 1s, calculate the number of allowed permutations by first
-- finding all the possible permutations, then filtering for the allowed ones,
-- then finding the length of the remaining list
allowedPerms :: Int -> Int
allowedPerms = length . filter allowed . permutations

-- Calculate all the possible adapter arrangements from the differences in jolts
-- between the arrangements.
--
-- We know that:
--
-- 1. There must be a difference of 1, 2 or three between adjacent adapters
-- 2. There must be a continuous chain from 0 to the phone socket value
--
-- This function is based on a few facts that one can prove about the sorted
-- sequence of differences:
--
-- 1. There are only jumps of 1 or 3 in the chain of all sorted adapters. This
-- one is easy to check empirically
--
-- 2. If we have a sequence of jumps of 3s in the chain of all sorted adapters,
-- then that chain will have to be there in all the valid permutations. There's
-- simply no leeway to not use adapters or swap things around.
--
-- 3. If we have a sequence of jumps of 1s, then we know that the last adapter
-- in the sequence *must* be there, since it will be followed by a three. So we
-- can discard it from the permutation combinations
--
--  Ok, so the algorithm works something like this:
--
--  1. Find the sequence of sumps from the sorted list of all adapters
--
--  diffs [1, 4, 5, 6, 7, 10, 11, 12, 15, 16, 19]
--  
--  >> [1, 3, 1, 1, 1, 3, 1, 1, 3, 1, 3]
--
--  2. Split the sequence into groups of 1s and 3s using the `group` function.
--
--  group [1, 3, 1, 1, 1, 3, 1, 1, 3, 1, 3]
--
--  >> [[1], [3], [1, 1, 1], [3], [1, 1], [3], [1], [3]]
--
-- 3. Throw away the groups of 3s, since those adapters have to be there and
-- they won't contribute to the total number of possible combinations
--
--  filter (elem 1) [[1], [3], [1, 1, 1], [3], [1, 1], [3], [1], [3]]
--
--  >> [[1], [1, 1, 1], [1, 1], [1]]
--
-- 4. For each group of 1s:
--
-- 4a. Find it's length
--
-- length [1, 1, 1]
--
-- >> 3
--
-- 4b. Subtract one, since the last one is also always fixed
--
-- pred 3
--
-- >> 2
--
-- 4c. Find all the allowed permutations for the remaining group of 1s
--
-- allowedPerms 2
--
-- >> 4
--
-- 5. Multiply the number of allowed permutations from all the groups together
--
-- product [1, 4, 2, 1]
--
-- >> 8
--
-- TADA! We're done
--
arrangements :: [Int] -> Int
arrangements =
  product . map (allowedPerms . pred . length) . filter (elem 1) . group . diffs

main :: IO ()
main = do
  nums <- fmap (map read . lines) . readFile =<< getDataFileName "inputs/day10"
  print (onesTimesThrees nums)
  print (arrangements nums)
