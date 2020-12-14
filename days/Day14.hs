{-# LANGUAGE RecordWildCards #-}

module Main where

import Paths_aoc2020

import Control.Applicative
import Data.List
import Data.List.Split
import qualified Data.Bits.Bitwise as B
import qualified Data.Map as M
import qualified Data.Set as S

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

memSum :: DecodeState -> Int
memSum = sum . M.elems . mem



type Address = [Bool]
type AddressMap = M.Map Int (S.Set Address)
data DecodeStateV2 = DecodeStateV2
  { maskV2 :: [MaskBit]
  , addrMap :: AddressMap
 }
  deriving Show

space :: [MaskBit] -> Address -> S.Set Address
space mask addr = S.fromList . go $ zip mask addr
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
      newAddrMap = M.insert value addrSpace (M.map (S.\\ addrSpace) addrMap)
  in DecodeStateV2 mask newAddrMap

memSumV2 :: DecodeStateV2 -> Integer
memSumV2
  = M.foldrWithKey (\k v i -> (fromIntegral k) * (fromIntegral v) + i) 0
  . M.map (fromIntegral . S.size)
  . addrMap

runV2 :: [Command] -> DecodeStateV2
runV2 = foldl' (flip stepV2) (DecodeStateV2 [] M.empty)

test :: [Command]
test = parse . unlines $
  [ "mask = 000000000000000000000000000000X1001X"
  , "mem[42] = 100"
  , "mask = 00000000000000000000000000000000X0XX"
  , "mem[26] = 1"
  ]

test2 :: [Command]
test2 = parse . unlines $
  [ "mask = 1110X1110XXX101X0011010X110X10X0110X"
  , "mem[40257] = 51331021"
  ]

showAddrSpace :: S.Set [Bool] -> String
showAddrSpace =
  unlines . map (map (\x -> if x then '1' else '0')) . reverse . S.toList

showKeyValue :: (Int, S.Set [Bool]) -> String
showKeyValue (k, v) = unlines ["---", show k, showAddrSpace v]

showMap :: M.Map Int (S.Set [Bool]) -> String
showMap = unlines . map showKeyValue . M.toList

main :: IO ()
main = do
  commands <- fmap parse . readFile =<< getDataFileName "inputs/day14"
  mapM_ print commands
  print (memSum $ run commands)
  putStrLn "---"
  putStrLn . showAddrSpace $
    space
      (reverse [M0, MX, M1, M0, M0, M1, MX])
      (reverse [False, True, False, True, False, True, False])
  putStrLn "---"
  putStrLn (showMap . addrMap $ runV2 test)
  print (memSumV2 $ runV2 test)
  print (memSumV2 $ runV2 test2)
  print (memSumV2 $ runV2 commands)
