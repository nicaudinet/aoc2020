{-# LANGUAGE RecordWildCards #-}

module Main where

import Paths_aoc2020

import Data.List.Split

parsePlayer :: String -> [Int]
parsePlayer = map read . tail . lines

parse :: String -> ([Int], [Int])
parse str =
  let [p1,p2] = splitOn "\n\n" str
  in (parsePlayer p1, parsePlayer p2)

combatStep :: ([Int], [Int]) -> Either ([Int], [Int]) [Int]
combatStep ([],[]) = error "something terrible happened"
combatStep ([], p2) = Right p2
combatStep (p1, []) = Right p1
combatStep (p1, p2) =
  let h1 = head p1
      h2 = head p2
  in
    if h1 > h2
    then Left (tail p1 <> [h1, h2], tail p2)
    else Left (tail p1, tail p2 <> [h2, h1])

combat :: ([Int],[Int]) -> [Int]
combat state =
  case combatStep state of
    Right p -> p
    Left s -> combat s

score :: [Int] -> Int
score = sum . zipWith (*) [1..] . reverse

data Player = P1 | P2
  deriving Show

data GameState = GameState
  { player1 :: [Int]
  , player2 :: [Int]
  , pastStates :: [([Int],[Int])]
  }

recCombatStep :: [Int] -> [Int] -> Either Player Player
recCombatStep [] [] = error "something terrible happened"
recCombatStep p1 [] = Right P1
recCombatStep [] p2 = Right P2
recCombatStep p1 p2 = Left $
  if h1 <= length t1 && h2 <= length t2
  then fst $ recGame (GameState (take h1 t1) (take h2 t2) [])
  else if h1 > h2 then P1 else P2
  where
    (h1, t1) = (head p1, tail p1)
    (h2, t2) = (head p2, tail p2)

recGame :: GameState -> (Player, [Int])
recGame GameState{..} =
  if elem (player1, player2) pastStates
  then (P1, player1)
  else
    case recCombatStep player1 player2 of
      Right P1 -> (P1, player1)
      Right P2 -> (P2, player2)
      Left P1 -> recGame (newGameState (t1 <> [h1, h2]) t2)
      Left P2 -> recGame (newGameState t1 (t2 <> [h2, h1]))
  where
    (h1, t1) = (head player1, tail player1)
    (h2, t2) = (head player2, tail player2)

    newGameState :: [Int] -> [Int] -> GameState
    newGameState a b = GameState a b ((player1, player2):pastStates)

recCombat :: ([Int], [Int]) -> [Int]
recCombat (p1, p2) = snd $ recGame (GameState p1 p2 [])

test :: ([Int], [Int])
test = parse $ unlines
  [ "Player 1:"
  , "9"
  , "2"
  , "6"
  , "3"
  , "1"
  , ""
  , "Player 2:"
  , "5"
  , "8"
  , "4"
  , "7"
  , "10"
  ]

main :: IO ()
main = do
  players <- fmap parse . readFile =<< getDataFileName "inputs/day22"
  print (score $ combat players)
  print (score $ recCombat players)
