module Main where

import Paths_aoc2020

import Data.Maybe (catMaybes)
import Data.Bifunctor (second)

data Seat = Floor | Empty | Full
  deriving (Show, Eq)

parseSeat :: Char -> Seat
parseSeat '.' = Floor
parseSeat 'L' = Empty
parseSeat '#' = Full

parse :: String -> [[Seat]]
parse = map (map parseSeat) . lines

indexes :: [[a]] -> [[(a, (Int, Int))]]
indexes grid = map fn (zip grid [0..])
  where
    fn :: ([a], Int) -> [(a, (Int, Int))]
    fn (xs, y) = map (gn y) (zip xs [0..])

    gn :: Int -> (a, Int) -> (a, (Int, Int))
    gn y (a, x) = (a, (x, y))

atCoord :: [[a]] -> (Int, Int) -> Maybe a
atCoord grid (x, y)
  | x < 0 = Nothing
  | y < 0 = Nothing
  | y >= length grid = Nothing
  | x >= length (head grid) = Nothing
  | otherwise = Just $ (grid !! y) !! x

(!?) :: [[a]] -> (Int, Int) -> Maybe a
(!?) = atCoord
infixl 9 !?

headMaybe :: [a] -> Maybe a
headMaybe [] = Nothing
headMaybe (x:xs) = Just x

type Adjacent = [[Seat]] -> (Int, Int) -> [Seat]

adjacentPart1 :: Adjacent
adjacentPart1 grid (w,h) =
  catMaybes
    $  [ grid !? (x, y) | x <- [w-1, w, w+1], y <- [h-1, h+1] ]
    <> [ grid !? (w-1,h), grid !? (w+1, h) ]

adjacentPart2 :: Adjacent
adjacentPart2 grid (w,h) = catMaybes $ map fn directionIndices
  where
    maxWidth :: Int
    maxWidth = length (head grid)

    maxHeight :: Int
    maxHeight = length grid

    north :: [(Int, Int)]
    north = [ (w, y) | y <- [h-1, h-2 .. 0] ]

    northEast :: [(Int, Int)]
    northEast = zip [w+1 .. maxWidth] [h-1, h-2 .. 0]

    east :: [(Int, Int)]
    east = [ (x, h) | x <- [w+1 .. maxWidth] ]

    southEast :: [(Int, Int)]
    southEast = zip [w+1 .. maxWidth] [h+1 .. maxHeight]

    south :: [(Int, Int)]
    south = [ (w, y) | y <- [h+1 .. maxHeight] ]

    southWest :: [(Int, Int)]
    southWest = zip [w-1, w-2 .. 0] [h+1 .. maxHeight]

    west :: [(Int, Int)]
    west = [ (x, h) | x <- [w-1, w-2 .. 0] ]

    northWest :: [(Int, Int)]
    northWest = zip [w-1, w-2 .. 0] [h-1, h-2 .. 0]

    directionIndices :: [[(Int, Int)]]
    directionIndices =
      [north, northEast, east, southEast, south, southWest, west, northWest]

    fn :: [(Int, Int)] -> Maybe Seat
    fn = headMaybe . filter (/= Floor) . catMaybes . map (grid !?)

adjacents :: Adjacent -> [[Seat]] -> [[(Seat, [Seat])]]
adjacents adjacent grid = map (map (second (adjacent grid))) (indexes grid)

type Rule = (Seat, [Seat]) -> Seat

rulePart1 :: Rule
rulePart1 (seat, adjs)
  | seat == Empty && not (elem Full adjs) = Full
  | seat == Full && length (filter (== Full) adjs) >= 4 = Empty
  | otherwise = seat

rulePart2 :: Rule
rulePart2 (seat, adjs)
  | seat == Empty && not (elem Full adjs) = Full
  | seat == Full && length (filter (== Full) adjs) >= 5 = Empty
  | otherwise = seat

step :: Adjacent -> Rule -> [[Seat]] -> [[Seat]]
step adjacent rule = map (map rule) . adjacents adjacent

fix :: Eq a => (a -> a) -> a -> a
fix f x =
  let x' = f x
  in if x == x' then x else fix f x'

run :: Adjacent -> Rule -> [[Seat]] -> [[Seat]]
run adjacent rule = fix (step adjacent rule)

occupied :: [[Seat]] -> Int
occupied = length . filter (== Full) . concat

main :: IO ()
main = do
  seats <- fmap parse . readFile =<< getDataFileName "inputs/day11"
  print (occupied (run adjacentPart1 rulePart1 seats))
  print (occupied (run adjacentPart2 rulePart2 seats))
