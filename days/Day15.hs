{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Paths_aoc2020

import Control.DeepSeq
import Data.List.Split (splitOn)
import qualified Data.Map as M
import GHC.Generics

parse :: String -> [Int]
parse = map read . splitOn ","

data GameState = GameState
  { turn :: Int
  , prev :: Int
  , prevMap :: M.Map Int Int
  }
  deriving (Show, Generic, NFData)

step :: GameState -> GameState
step GameState{..} =
  let spokenNum = maybe 0 (pred . (turn -)) (M.lookup prev prevMap)
  in GameState (turn + 1) spokenNum (M.insert prev (turn - 1) prevMap)

initGameState :: [Int] -> GameState
initGameState initValues =
  GameState
    { turn = length initValues + 1
    , prev = last initValues
    , prevMap = M.fromList (zip (init initValues) [1..])
    }

applyNStrict :: Int -> (GameState -> GameState) -> GameState -> GameState
applyNStrict 0 _ n = n
applyNStrict accum fn n = applyNStrict (accum - 1) fn $!! (fn n)

run :: [Int] -> Int -> Int
run initValues stop =
  prev $ applyNStrict (stop - initTurns) step (initGameState initValues)
  where
    initTurns :: Int
    initTurns = length initValues

main :: IO ()
main = do
  nums <- fmap parse . readFile =<< getDataFileName "inputs/day15"
  -- putStrLn "--- Test ---"
  -- print (initGameState [0,3,6])
  -- print (step $ initGameState [0,3,6])
  -- print (step . step $ initGameState [0,3,6])
  -- print (step . step . step $ initGameState [0,3,6])
  -- print (step . step . step . step $ initGameState [0,3,6])
  -- print (run [3,1,2] 2020)
  -- mapM_ print [0,3]
  -- mapM_ print (map prev . take 100 . iterate step $ initGameState [0, 3, 6])
  -- putStrLn "--- Real ---"
  -- print (run nums 2020)
  print (run nums 300000)
