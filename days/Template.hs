module Main where

import Paths_aoc2020

main :: IO ()
main = do
  content <- readFile =<< getDataFileName "inputs/day3"
  putStrLn content
