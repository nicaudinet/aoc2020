module Main where

import Paths_aoc2020
import Data.Char

parse :: String -> (Int, Int, Char, String)
parse input =
  let
    (n1, '-':rest) = span isDigit input
    (n2, ' ':char:':':' ':str) = span isDigit rest
  in (read n1, read n2, char, str)

check :: (Int, Int, Char, String) -> Bool
check (n1, n2, char, str) =
  let occurrences = length $ filter (== char) str
  in occurrences >= n1 && occurrences <= n2

check2 :: (Int, Int, Char, String) -> Bool
check2 (n1, n2, char, str) =
  let chars = [str !! (n1 - 1), str !! (n2 - 1)]
  in length (filter (== char) chars) == 1

main :: IO ()
main = do
  content <- readFile =<< getDataFileName "inputs/day2"
  let parsed = map parse (lines content)
  print (length $ filter check parsed)
  print (length $ filter check2 parsed)
