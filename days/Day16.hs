{-# LANGUAGE RecordWildCards #-}

module Main where

import Paths_aoc2020
import Text.Parsec
import Data.List
import Data.Maybe

import Debug.Trace

data Rule = Rule
  { ruleName :: String
  , ruleRange :: [Integer]
  }
  deriving Show

type Ticket = [Integer]

data Info = Info
  { rules :: [Rule]
  , myTicket :: Ticket
  , tickets :: [Ticket]
  }
  deriving Show

range :: Parsec String u [Integer]
range = do
  [n1, n2] <- sepBy (many digit) (char '-')
  pure [read n1 .. read n2]

rule :: Parsec String u Rule
rule = do
  name <- manyTill anyChar (char ':')
  space
  r1 <- range
  string " or "
  r2 <- range
  newline
  pure (Rule name (r1 <> r2))

ticket :: Parsec String u Ticket
ticket = do
  ns <- sepBy (many digit) (char ',')
  newline
  pure (map read ns)

emptyLine :: Parsec String u ()
emptyLine = newline >> pure ()

parseInfo :: Parsec String u Info
parseInfo = do
  rules <- manyTill rule emptyLine
  string "your ticket:"
  newline
  myTicket <- ticket
  emptyLine
  string "nearby tickets:"
  newline
  tickets <- many ticket
  pure Info{..}

parseInput :: String -> Info
parseInput str =
  let Right info = parse parseInfo "" str
  in info

validNums :: [Rule] -> [Integer]
validNums = concat . map ruleRange

invalidNearbyTickets :: Info -> Integer
invalidNearbyTickets Info{..} =
  let validSet = concat (map ruleRange rules)
  in sum $ filter (not . flip elem validSet) (concat tickets)

discardInvalid :: Info -> Info
discardInvalid Info{..} =
  let validSet = concat (map ruleRange rules)
      isValidTicket = and . map (flip elem validSet)
  in Info rules myTicket (filter isValidTicket tickets)

data TicketColumn = TicketColumn
  { columnValues :: [Integer]
  , columnPosition :: Int
  }
  deriving Show

data IndexedRule = IndexedRule Rule Int

toColumns :: [Ticket] -> [TicketColumn]
toColumns tickets = zipWith TicketColumn (transpose tickets) [0..]

while :: (a -> Maybe b) -> [a] -> b
while fn [] = error "reached end of list in while function"
while fn (x:xs) =
  case fn x of
    Just x -> x
    Nothing -> while fn xs

match :: [Integer] -> Rule -> Bool
match column (Rule _name range) = and $ map (flip elem range) column

deleteAtIndex :: Int -> [a] -> [a]
deleteAtIndex n xs = take n xs <> drop (n + 1) xs

matchRule :: [Rule] -> TicketColumn -> Maybe (IndexedRule, [Rule])
matchRule rules (TicketColumn xs idx) = do
  let matchingRules = filter (match xs . fst) (zip rules [0..])
  (r, ruleIdx) <-
    if length matchingRules > 1
    then Nothing
    else Just (head matchingRules)
  pure (IndexedRule r idx, deleteAtIndex ruleIdx rules)

nextRule :: ([Rule], [TicketColumn]) -> Maybe (IndexedRule, ([Rule], [TicketColumn]))
nextRule ([], []) = Nothing
nextRule (rules, cols) = -- traceShow rules $ traceShow cols $ trace "---" $
  let (r@(IndexedRule _ idx), remainingRules) = while (matchRule rules) cols
  in Just (r, (remainingRules, filter ((/= idx) . columnPosition) cols))

orderRules :: [Rule] -> [TicketColumn] -> [Rule]
orderRules rules cols
  = map (\(IndexedRule rule _) -> rule)
  . sortOn (\(IndexedRule _ n) -> n)
  $ unfoldr nextRule (rules, cols)

departures :: [Rule] -> Ticket -> Integer
departures rules ticket =
  let isDepartureRule (Rule name _range) = "departure" `isPrefixOf` name
  in product . map snd . filter (isDepartureRule . fst) $ zip rules ticket

part2 :: Info -> Integer
part2 info =
  let validInfo = discardInvalid info
      orderedRules = orderRules (rules info) (toColumns $ tickets validInfo)
  in departures orderedRules (myTicket info)

test :: Info
test = parseInput $ unlines
  [ "class: 0-1 or 4-19"
  , "row: 0-5 or 8-19"
  , "seat: 0-13 or 16-19"
  , ""
  , "your ticket:"
  , "11,12,13"
  , ""
  , "nearby tickets:"
  , "3,9,18"
  , "15,1,5"
  , "5,14,9"
  ]

main :: IO ()
main = do
  info <- fmap parseInput . readFile =<< getDataFileName "inputs/day16"
  print (invalidNearbyTickets info)
  print (part2 info)
