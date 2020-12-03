module Main where

import Paths_aoc2020

testMap :: Map
testMap = parseMap $ unlines
  [ "..##......."
  , "#...#...#.."
  , ".#....#..#."
  , "..#.#...#.#"
  , ".#...##..#."
  , "..#.##....."
  , ".#.#.#....#"
  , ".#........#"
  , "#.##...#..."
  , "#...##....#"
  , ".#..#...#.#"
  ]

data Square = Tree | Open
  deriving (Show, Eq)

type Map = [[Square]]

parseMap :: String -> Map
parseMap =
  let parseSquare c = if c == '#' then Tree else Open
  in map (concat . repeat . map parseSquare) . lines

route :: Map -> (Int, Int) -> [(Int, Int)]
route map (right, down) =
  [ ((y * right `div` down), y) | y <- [down, down + down .. length map - 1] ]

trees :: Map -> (Int, Int) -> Int
trees m =
  let getSquare (x,y) = (m !! y) !! x
  in length . filter (== Tree) . map getSquare . route m

main :: IO ()
main = do
  map <- fmap parseMap . readFile =<< getDataFileName "inputs/day3"
  print (trees map (3, 1))
  let routes = [(1,1), (3,1), (5,1), (7,1), (1,2)]
  print (product (fmap (trees testMap) routes))
  print (product (fmap (trees map) routes))
