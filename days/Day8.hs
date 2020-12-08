{-# LANGUAGE RecordWildCards #-}

module Main where

import Paths_aoc2020

data Instruction
  = Nop Int
  | Acc Int
  | Jmp Int
  deriving Show

parseInt :: String -> Int
parseInt ('+':int) = read int
parseInt int = read int

parseInstruction :: String -> Instruction
parseInstruction str =
  case words str of
    ["nop", num ] -> Nop (parseInt num)
    ["acc", num] -> Acc (parseInt num)
    ["jmp", num] -> Jmp (parseInt num)

parseProgram :: String -> [Instruction]
parseProgram = map parseInstruction . lines

data ProgramState = ProgramState
  { stateAcc :: Int
  , statePc :: Int
  , statePastPc :: [Int]
  }
  deriving Show

eval :: Instruction -> (Int, Int) -> (Int, Int)
eval (Nop n) (acc, pc) = (acc, pc + 1)
eval (Acc n) (acc, pc) = (acc + n, pc + 1)
eval (Jmp n) (acc, pc) = (acc, pc + n)

data RunState
  = InfiniteLoop Int
  | NextStep ProgramState
  | Terminates Int
  | ErrorPcTooLarge

step :: [Instruction] -> ProgramState -> RunState
step program ps@ProgramState{..} =
  if statePc >= length program
  then
    if statePc == length program
    then Terminates stateAcc
    else ErrorPcTooLarge
  else
    let (newAcc, newPc) = eval (program !! statePc) (stateAcc, statePc)
    in
      if elem newPc statePastPc
      then InfiniteLoop newAcc
      else NextStep $ ProgramState newAcc newPc (statePc : statePastPc)

data Result
  = Loop Int
  | Exit Int
  | Error
  deriving Show

run :: [Instruction] -> Result
run program =
  let initState = ProgramState 0 0 []
  in go program initState
  where
    go :: [Instruction] -> ProgramState -> Result
    go program state =
      case step program state of
        InfiniteLoop acc -> Loop acc
        Terminates acc -> Exit acc
        NextStep newState -> go program newState
        ErrorPcTooLarge -> Error

swap :: Instruction -> Instruction
swap (Nop n) = Jmp n
swap (Acc n) = Acc n
swap (Jmp n) = Nop n

swapAt :: Int -> [Instruction] -> [Instruction]
swapAt n program =
  take n program <> [swap $ program !! n] <> drop (n + 1) program

fixedRun :: [Instruction] -> Int
fixedRun program = go 0 program
  where
    go :: Int -> [Instruction] -> Int
    go swapCounter program =
      case run program of
        Exit n -> n
        _otherwise ->
          case run (swapAt swapCounter program) of
            Exit n -> n
            _otherwise ->
              go (swapCounter + 1) program

test :: [Instruction]
test = parseProgram $ unlines
  [ "nop +0"
  , "acc +1"
  , "jmp +4"
  , "acc +3"
  , "jmp -3"
  , "acc -99"
  , "acc +1"
  , "jmp -4"
  , "acc +6"
  ]

main :: IO ()
main = do
  program <- fmap parseProgram . readFile =<< getDataFileName "inputs/day8"
  print (run test)
  print (run program)
  print (fixedRun test)
  print (fixedRun program)
