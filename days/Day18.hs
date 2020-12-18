module Main where

import Paths_aoc2020

import Text.Parsec hiding (space, spaces)

data Expr
  = Val Int
  | Add Expr Expr
  | Mul Expr Expr
  deriving Show

-- number ::= [ "-" ] digit { digit }
-- digit  ::= "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"
-- expr   ::= term { op term }
-- term   ::= "(" expr ")" | number
-- op     ::= "+" | "*"

space :: Parsec String u ()
space = char ' ' >> pure ()

spaces :: Parsec String u ()
spaces = many space >> pure ()

val :: Parsec String u Expr
val = do
  num <- many digit
  spaces
  pure (Val $ read num)

parens :: Parsec String u a -> Parsec String u a
parens = between open close
  where
    open  = char '(' >> spaces
    close = char ')' >> spaces

add :: Parsec String u (Expr -> Expr -> Expr)
add = do
  char '+'
  spaces
  pure Add

mul :: Parsec String u (Expr -> Expr -> Expr)
mul = do
  char '*'
  spaces
  pure Mul

termSame :: Parsec String u Expr
termSame = parens exprSame <|> val

exprSame :: Parsec String u Expr
exprSame = chainl1 termSame (add <|> mul)

parseAllSame :: String -> [Expr]
parseAllSame =
  either (error "parse error") id . parse (endBy exprSame newline) ""

termDiff :: Parsec String u Expr
termDiff = parens exprDiff <|> val

factor :: Parsec String u Expr
factor = termDiff `chainl1` add

exprDiff :: Parsec String u Expr
exprDiff = chainl1 factor mul

parseAllDiff :: String -> [Expr]
parseAllDiff =
  either (error "parse error") id . parse (endBy exprDiff newline) ""

eval :: Expr -> Int
eval (Val n) = n
eval (Add e1 e2) = eval e1 + eval e2
eval (Mul e1 e2) = eval e1 * eval e2

sumAll :: [Expr] -> Int
sumAll = sum . map eval

test1 :: String
test1 = "1 + 2 * 3 + 4 * 5 + 6"

test2 :: String
test2 = "1 + (2 * 3) + (4 * (5 + 6))"

test3 :: String
test3 = "1234\n1234\n1234"

main :: IO ()
main = do
  exprsSame <- fmap parseAllSame . readFile =<< getDataFileName "inputs/day18"
  exprsDiff <- fmap parseAllDiff . readFile =<< getDataFileName "inputs/day18"
  print (sumAll exprsSame)
  print (sumAll exprsDiff)
