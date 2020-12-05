module Main where

import Paths_aoc2020
import Data.Monoid

data Bin = One | Zero
  deriving Show

findBin :: [Bin] -> Int
findBin bins = getSum . foldMap fn . zip bins $ reverse [0 .. length bins - 1]
  where
    fn :: (Bin, Int) -> Sum Int
    fn (Zero, _) = 0
    fn (One, n) = Sum (2 ^ n)

findRow :: String -> Int
findRow = findBin . map (\c -> if c == 'F' then Zero else One)

findCol :: String -> Int
findCol = findBin . map (\c -> if c == 'L' then Zero else One)

parseSeat :: String -> (Int, Int)
parseSeat str = (findRow (take 7 str), findCol (drop 7 str))

seatId :: (Int, Int) -> Int
seatId (row, col) = row * 8 + col

parseSeatIds :: String -> [Int]
parseSeatIds = map (seatId . parseSeat) . lines

findSeat :: [Int] -> Int
findSeat seatIds =
  snd . head . filter fst $ map (checkSeat seatIds) [0 .. 128 * 8]
  where
    checkSeat :: [Int] -> Int -> (Bool, Int)
    checkSeat seatIds seat =
      let 
        b =    (not $ elem seat seatIds)
            && elem (seat - 1) seatIds
            && elem (seat + 1) seatIds
      in
        (b, seat)

main :: IO ()
main = do
  seatIds <- fmap parseSeatIds . readFile =<< getDataFileName "inputs/day5"
  print (maximum seatIds)
  print (findSeat seatIds)
