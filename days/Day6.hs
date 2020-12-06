module Main where

import Paths_aoc2020
import qualified Data.Set as S
import Data.List.Split (splitOn)

groups :: String -> [[S.Set Char]]
groups = map (map S.fromList . lines) . splitOn "\n\n"

orQuestions :: [[S.Set Char]] -> [S.Set Char]
orQuestions = map S.unions

andQuestions :: [[S.Set Char]] -> [S.Set Char]
andQuestions = map (\xs -> foldr S.intersection (S.unions xs) xs)

sumGroups :: [S.Set Char] -> Int
sumGroups = sum . map (length . S.toList)

main :: IO ()
main = do
  gs <- fmap groups . readFile =<< getDataFileName "inputs/day6"
  print (sumGroups $ orQuestions gs)
  print (sumGroups $ andQuestions gs)
