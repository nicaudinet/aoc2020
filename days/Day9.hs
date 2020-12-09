module Main where

import Paths_aoc2020

import Data.List
import qualified Data.Set as S

parse :: String -> [Int]
parse = map read . lines

checkXmas :: [Int] -> Int -> Int
checkXmas nums preambleSize = go preambleSize
  where
    preamble :: Int -> [Int]
    preamble = take preambleSize . reverse . flip take nums

    correct :: Int -> [Int] -> Bool
    correct n ns = elem n [ x + y | x <- ns, y <- ns, x /= y ]

    go :: Int -> Int
    go idx =
      if not $ correct (nums !! idx) (preamble idx)
      then nums !! idx
      else go (idx + 1)

contiguousSum :: Int -> [Int] -> [Int]
contiguousSum n = head . filter ((== n) . sum) . contiguousSets
  where
    contiguous :: [a] -> Int -> [[a]]
    contiguous ns size =
      if size > length ns
      then []
      else take size ns : contiguous (tail ns) size

    contiguousSets :: [a] -> [[a]]
    contiguousSets ns = concat $ map (contiguous ns) [2..]

test :: [Int]
test =
  [ 35
  , 20
  , 15
  , 25
  , 47
  , 40
  , 62
  , 55
  , 65
  , 95
  , 102
  , 117
  , 150
  , 182
  , 127
  , 219
  , 299
  , 277
  , 309
  , 576
  ]

main :: IO ()
main = do
  nums <- fmap parse . readFile =<< getDataFileName "inputs/day9"

  let testFailNum = checkXmas test 5
      testXs = contiguousSum testFailNum test
  putStrLn "-- Test --"
  print testFailNum
  print (minimum testXs + maximum testXs)

  let failNum = checkXmas nums 25
      xs = contiguousSum failNum nums
  putStrLn "-- Real --"
  print failNum
  print (minimum xs + maximum xs)
