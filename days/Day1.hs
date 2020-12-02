{-# LANGUAGE TypeApplications #-}

module Main where

import Paths_aoc2020

main :: IO ()
main = do
  file <- getDataFileName "inputs/day1"
  nums <- map (read @Int) . lines <$> readFile file
  print $ head [ a * b | a <- nums, b <- nums, a + b == 2020 ]
  print $ head [ a * b * c | a <- nums, b <- nums, c <- nums, a + b + c == 2020 ]
