{-# LANGUAGE RecordWildCards #-}

module Main where

import Paths_aoc2020

import Control.Applicative
import Data.List
import Data.List.Split
import qualified Data.Bits.Bitwise as B
import qualified Data.Map as M
import GHC.Exts (sortWith)

data MaskBit = M1 | M0 | MX
  deriving Show

parseMaskBit :: Char -> MaskBit
parseMaskBit 'X' = MX
parseMaskBit '1' = M1
parseMaskBit '0' = M0
parseMaskBit _ = error "strange mask bit"

parseMask :: String -> Maybe Command
parseMask str
  | isPrefixOf "mask = " str =
      Just . Mask . map parseMaskBit $ drop (length "mask = ") str
  | otherwise = Nothing

type MemAddress = (Int, Int) 

parseMem :: String -> Maybe Command
parseMem str
  | isPrefixOf "mem[" str =
      let [addr, value] = splitOn " = " str
      in Just $ Mem (read (drop (length "mem[") (init addr)), read value)
  | otherwise = Nothing

data Command = Mask [MaskBit] | Mem MemAddress
  deriving Show

parseLine :: String -> Command
parseLine = maybe (error "failed parse") id . (\s -> parseMask s <|> parseMem s)

parse :: String -> [Command]
parse = map parseLine . lines



data DecodeState = DecodeState
  { mask :: [MaskBit]
  , mem :: M.Map Int Int
  }
  deriving Show

applyMaskBit :: (MaskBit, Bool) -> Bool
applyMaskBit (MX, b) = b
applyMaskBit (M1, b) = True
applyMaskBit (M0, b) = False

asBitsLE :: Int -> ([Bool] -> [Bool]) -> Int
asBitsLE num fn = B.fromListLE (fn (B.toListLE num))

applyMask :: [MaskBit] -> Int -> Int
applyMask mask num = asBitsLE num (map applyMaskBit . zip (reverse mask))

step :: Command -> DecodeState -> DecodeState
step (Mask bits) state = state { mask = bits }
step (Mem (addr, num)) DecodeState{..} =
  DecodeState mask (M.insert addr (applyMask mask num) mem)

run :: [Command] -> DecodeState
run = foldl' (flip step) (DecodeState [] M.empty)

memSum :: M.Map key Int -> Int
memSum = sum . M.elems



type Address = [Bool]
type AddressMap = M.Map Address Int
data DecodeStateV2 = DecodeStateV2
  { maskV2 :: [MaskBit]
  , addrMap :: AddressMap
 }
  deriving Show

space :: [MaskBit] -> Address -> [Address]
space mask addr = go $ zip mask addr
  where
    go :: [(MaskBit, Bool)] -> [Address]
    go [] = [[]]
    go ((M0,  bit):xs) = map (bit  :) (go xs)
    go ((M1, _bit):xs) = map (True :) (go xs)
    go ((MX, _bit):xs) =
      let prev = go xs
      in map (True :) prev <> map (False :) prev

stepV2 :: Command -> DecodeStateV2 -> DecodeStateV2
stepV2 (Mask bits) (DecodeStateV2 _ addrMap) = DecodeStateV2 bits addrMap
stepV2 (Mem (addr, value)) (DecodeStateV2 mask addrMap) =
  let addrSpace = space (reverse mask) (B.toListLE addr)
      newAddrMap = foldr (\k -> M.insert k value) addrMap addrSpace
  in DecodeStateV2 mask newAddrMap

runV2 :: [Command] -> DecodeStateV2
runV2 = foldl' (flip stepV2) (DecodeStateV2 [] M.empty)

test :: [Command]
test = parse . unlines $
  [ "mask = 000000000000000000000000000000X1001X"
  , "mem[42] = 100"
  , "mask = 00000000000000000000000000000000X0XX"
  , "mem[26] = 1"
  ]

showAddr :: [Bool] -> String
showAddr = map (\x -> if x then '1' else '0')

showKeyValue :: ([Bool], Int) -> String
showKeyValue (k, v) = unwords [showAddr k, "->", show v]

showMap :: M.Map [Bool] Int -> String
showMap = unlines . map showKeyValue . sortWith snd . M.toList

main :: IO ()
main = do
  commands <- fmap parse . readFile =<< getDataFileName "inputs/day14"
  putStrLn "--- Test ---"
  putStrLn (showMap . addrMap $ runV2 test)
  print (memSum . addrMap $ runV2 test)
  putStrLn "--- Real ---"
  print (memSum . mem $ run commands)
  print (memSum . addrMap $ runV2 commands)
