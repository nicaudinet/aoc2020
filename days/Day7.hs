module Main where

import Paths_aoc2020
import Data.List.Split

import qualified Data.Map as M

type Rules = M.Map String [(Int, String)]

parseBag :: String -> (Int, String)
parseBag str =
  let (num:bag) = words str
  in (read num, unwords (init bag))

parseBags :: String -> [(Int, String)]
parseBags " no other bags." = []
parseBags str = map parseBag (splitOn "," str)

parseRule :: String -> (String, [(Int, String)])
parseRule rule =
  let (bag:rest) = splitOn "contain" rule
  in ((unwords . init . words $ bag), parseBags (head rest))

parseRules :: String -> Rules
parseRules = M.fromList . map parseRule . lines

containsShinyGold :: Rules -> [Bool]
containsShinyGold rules = map contains (M.keys rules)
  where
    containsMap :: M.Map String Bool
    containsMap = fmap (fn . map snd) rules

    fn :: [String] -> Bool
    fn strs =
      if elem "shiny gold" strs
      then True
      else or (map contains strs)

    contains :: String -> Bool
    contains str = containsMap M.! str

numBags :: Rules -> Int
numBags rules = num "shiny gold"
  where
    numMap :: M.Map String Int
    numMap = fmap fn rules

    fn :: [(Int, String)] -> Int
    fn = sum . map (\(n, s) -> n + n * num s)

    num :: String -> Int
    num str = numMap M.! str

test :: String
test = unlines
  [ "light red bags contain 1 bright white bag, 2 muted yellow bags."
  , "dark orange bags contain 3 bright white bags, 4 muted yellow bags."
  , "bright white bags contain 1 shiny gold bag."
  , "muted yellow bags contain 2 shiny gold bags, 9 faded blue bags."
  , "shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags."
  , "dark olive bags contain 3 faded blue bags, 4 dotted black bags."
  , "vibrant plum bags contain 5 faded blue bags, 6 dotted black bags."
  , "faded blue bags contain no other bags."
  , "dotted black bags contain no other bags."
  ]

main :: IO ()
main = do
  rules <- fmap parseRules . readFile =<< getDataFileName "inputs/day7"
  putStrLn "-- Test --"
  let testRules = parseRules test
  mapM_ print (M.toList testRules)
  print (length $ filter id $ containsShinyGold testRules)
  print (numBags testRules)
  putStrLn "-- Real --"
  print (length $ filter id $ containsShinyGold rules)
  print (numBags rules)

