{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Paths_aoc2020

import Data.Bifunctor (second)
import Data.Maybe (listToMaybe, catMaybes)
import Data.List.Split (splitOn)

type Timestamp = Integer

data BusId = X | BusId Integer
  deriving (Show, Eq)

parseBusId :: String -> BusId
parseBusId "x" = X
parseBusId num = BusId (read num)

parse :: String -> [BusId]
parse = map parseBusId . splitOn ","

iterateUntilSuccess :: (a -> Maybe b) -> (a -> a) -> a -> b
iterateUntilSuccess compute step value =
  case compute value of
    Nothing -> iterateUntilSuccess compute step (step value)
    Just b -> b

nextBus :: [BusId] -> Timestamp -> (Timestamp, BusId)
nextBus busIds = iterateUntilSuccess compute succ
  where
    validBus :: Timestamp -> BusId -> Bool
    validBus _t X = False
    validBus t (BusId i) = t `mod` i == 0

    compute :: Timestamp -> Maybe (Timestamp, BusId)
    compute ts = (ts,) <$> listToMaybe (filter (validBus ts) busIds)

-- For part 2 we need to solve the Chinese Remainder Theorem
--
-- First we start by solving for x in the following set of equations:
--
-- x == a1 mod n1
-- x == a2 mod n2
--
-- where we know that n1 and n2 are coprime.
--
-- To solve this we can first use Extended Eclid's Algorithm on the ns, which
-- will return the gcd(n1, n2) as well as an m1 and m2 such that:
--
-- n1 * m1 + n2 * m2 = gcd(n1, n2)
-- 
-- Then we can calculate x from:
--  
-- x = a1 * m2 * n2 + a2 * m1 * n1
--
-- Once we can do it for two sets of equations, we can generalize for any number
-- of equations by:
--
-- * Solve x for two of the equations (as done above)
-- * Create a new equation x = a12 mod n12 where n12 = n1 * n2 and a12 = x mod
-- n12
-- * Add it back to the list of euqations
-- * Repeat until there is only one x left

data EuclidState = EuclidState
  { r1 :: Integer
  , r2 :: Integer
  , s1 :: Integer
  , s2 :: Integer
  , t1 :: Integer
  , t2 :: Integer
  }

-- The first integer must always be the larger one of the two
bezout :: Integer -> Integer -> (Integer, Integer)
bezout a b =
  iterateUntilSuccess compute step (EuclidState (max a b) (min a b) 1 0 0 1)
  where
    compute :: EuclidState -> Maybe (Integer, Integer)
    compute EuclidState{..} =
      if r2 == 0
      then Just (s1, t1)
      else Nothing

    step :: EuclidState -> EuclidState
    step EuclidState{..} =
      let
        q = r1 `div` r2
        r = r1 `mod` r2
        s = s1 - (s2 * q)
        t = t1 - (t2 * q)
      in EuclidState
        { r1 = r2
        , r2 = r
        , s1 = s2
        , s2 = s
        , t1 = t2
        , t2 = t
        }

remainder :: (Integer, Integer) -> (Integer, Integer) -> (Integer, Integer)
remainder (a1, n1) (a2, n2) =
  let
    (amax, nmax) = if n1 > n2 then (a1, n1) else (a2, n2)
    (amin, nmin) = if n1 < n2 then (a1, n1) else (a2, n2)
    (mmax, mmin) = bezout nmax nmin
    x = amin * mmax * nmax + amax * mmin * nmin
    n12 = n1 * n2
    a12 = x `mod` n12
  in
    (a12, n12)

remainders :: [(Integer, Integer)] -> (Integer, Integer)
remainders = foldl1 remainder

toMods :: [BusId] -> [(Integer, Integer)]
toMods = catMaybes . map toMod . zip [0..]
  where
    toMod :: (Integer, BusId) -> Maybe (Integer, Integer)
    toMod (_, X) = Nothing
    toMod (a, BusId n) = Just ((-a) `mod` n, n)

nextBusses :: [BusId] -> Timestamp
nextBusses = fst . remainders . toMods

test1 :: [BusId]
test1 = [BusId 17, X, BusId 13, BusId 19]

test2 :: [BusId]
test2 = map BusId [67,7,59,61]

main :: IO ()
main = do
  [timeString, busString] <- fmap lines . readFile =<< getDataFileName "inputs/day13"
  let timestamp = read timeString
      busIds = parse busString
      (busTime, BusId busId) = nextBus busIds timestamp
  putStrLn "--- Tests ---"
  print (bezout 161 28)
  print (bezout 28 161)
  putStrLn "-------"
  print (remainder (2,3) (3,5))
  putStrLn "-------"
  print (toMods test1)
  print (remainders $ toMods test1)
  print (nextBusses test1)
  putStrLn "-------"
  print (toMods test2)
  print (remainders $ toMods test2)
  print (nextBusses test2)
  putStrLn "--- Real ---"
  print (busId * (busTime - timestamp))
  print (nextBusses busIds)
