module Main where

import Paths_aoc2020

import Data.List (foldl')

data Command
  = N Int
  | E Int
  | S Int
  | W Int
  | L Int
  | R Int
  | F Int
  deriving Show

parseCommand :: String -> Command
parseCommand ('N':num) = N (read num)
parseCommand ('E':num) = E (read num)
parseCommand ('S':num) = S (read num)
parseCommand ('W':num) = W (read num)
parseCommand ('L':num) = L (read num)
parseCommand ('R':num) = R (read num)
parseCommand ('F':num) = F (read num)

parse :: String -> [Command]
parse = map parseCommand . lines

data Direction = North | East | South | West
  deriving (Show, Enum)

data ShipState = ShipState Int Int Direction
  deriving Show

turnLeft :: Int -> Direction -> Direction
turnLeft angle dir = toEnum $ (fromEnum dir - (angle `div` 90)) `mod` 4

turnRight :: Int -> Direction -> Direction
turnRight angle dir = toEnum $ (fromEnum dir + (angle `div` 90)) `mod` 4

stepShip :: Command -> ShipState -> ShipState
stepShip (N num) (ShipState x y dir) = ShipState x (y + num) dir
stepShip (E num) (ShipState x y dir) = ShipState (x + num) y dir
stepShip (S num) (ShipState x y dir) = ShipState x (y - num) dir
stepShip (W num) (ShipState x y dir) = ShipState (x - num) y dir
stepShip (L num) (ShipState x y dir) = ShipState x y (turnLeft num dir)
stepShip (R num) (ShipState x y dir) = ShipState x y (turnRight num dir)
stepShip (F num) (ShipState x y dir) =
  case dir of
    North -> ShipState x (y + num) dir
    East -> ShipState (x + num) y dir
    South -> ShipState x (y - num) dir
    West -> ShipState (x - num) y dir

runShip :: [Command] -> ShipState
runShip = foldl' (flip stepShip) (ShipState 0 0 East)

data ShipWaypoint = ShipWaypoint Int Int Int Int
  deriving Show

stepWaypoint :: Command -> ShipWaypoint -> ShipWaypoint
stepWaypoint (N num) (ShipWaypoint sx sy wx wy) =
  ShipWaypoint sx sy wx (wy + num)
stepWaypoint (E num) (ShipWaypoint sx sy wx wy) =
  ShipWaypoint sx sy (wx + num) wy
stepWaypoint (S num) (ShipWaypoint sx sy wx wy) =
  ShipWaypoint sx sy wx (wy - num)
stepWaypoint (W num) (ShipWaypoint sx sy wx wy) =
  ShipWaypoint sx sy (wx - num) wy
stepWaypoint (L num) (ShipWaypoint sx sy wx wy) =
-- ( cosA -sinA )
-- ( sinA  cosA )
  case num of
    90  -> ShipWaypoint sx sy (-wy) wx
    180 -> ShipWaypoint sx sy (-wx) (-wy)
    270 -> ShipWaypoint sx sy wy (-wx)
stepWaypoint (R num) (ShipWaypoint sx sy wx wy) =
-- ( cosA  sinA )
-- ( -sinA cosA )
  case num of
    90  -> ShipWaypoint sx sy wy (-wx)
    180 -> ShipWaypoint sx sy (-wx) (-wy)
    270 -> ShipWaypoint sx sy (-wy) wx
stepWaypoint (F num) (ShipWaypoint sx sy wx wy) =
  ShipWaypoint (sx + wx * num) (sy + wy * num) wx wy

runWaypoint :: [Command] -> ShipWaypoint
runWaypoint = foldl' (flip stepWaypoint) (ShipWaypoint 0 0 10 1)

test :: [Command]
test = map parseCommand [ "F10" , "N3" , "F7" , "R90" , "F11" ]

main :: IO ()
main = do
  commands <- fmap parse . readFile =<< getDataFileName "inputs/day12"
  print (runShip test)
  let ShipState x y _dir = runShip commands
  print (abs x + abs y)
  print (runWaypoint test)
  let ShipWaypoint sx sy _wx _wy = runWaypoint commands
  print (abs sx + abs sy)
