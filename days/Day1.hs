{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Paths_aoc2020
import Data.List (subsequences)

pairs :: [a] -> [(a,a)]
pairs [] = []
pairs (x:xs) = map (x,) xs <> pairs xs

triples :: [a] -> [(a,a,a)]
triples [] = []
triples (x:xs) = map (\(a,b) -> (x,a,b)) (pairs xs) <> triples xs

main :: IO ()
main = do
  file <- getDataFileName "inputs/day1"
  numbers <- map (read @Int) . lines <$> readFile file
  let [(a,b)] = filter (\(a,b) -> a + b == 2020) (pairs numbers)
  print (a * b)
  let [(x,y,z)] = filter (\(x,y,z) -> x + y + z == 2020) (triples numbers)
  print (x * y * z)
