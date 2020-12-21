module Main where

import Paths_aoc2020

import Text.Parsec hiding (runParser, spaces, token)

type Parser a = Parsec String () a

data Rule
  = RuleChar Char
  | Rules [Int]
  | Or Rule Rule
  deriving Show

spaces :: Parser ()
spaces = many (char ' ') >> pure ()

token :: Parser a -> Parser a
token p = do
  a <- p
  spaces
  pure a

number :: Parser Int
number = read <$> token (many1 digit)

charRule :: Parser Rule
charRule = do
  c <- between (char '"') (char '"') letter
  pure (RuleChar c)

rulesRule :: Parser Rule
rulesRule = Rules <$> many1 number

orRule :: Parser Rule
orRule = do
  r1 <- rulesRule
  token (char '|')
  r2 <- rulesRule
  pure (Or r1 r2)

ruleBody :: Parser Rule
ruleBody = charRule <|> try orRule <|> rulesRule

rule :: Parser (Int, Rule)
rule = do
  name <- number
  token (char ':')
  body <- ruleBody
  newline
  pure (name, body)

rules :: Parser [(Int, Rule)]
rules = many rule

messages :: Parser [String]
messages = many letter `endBy` newline

parseAll :: Parser ([(Int, Rule)], [String])
parseAll = do
  rs <- rules
  newline
  ms <- messages
  pure (rs, ms)

runParser :: String -> ([(Int, Rule)], [String])
runParser = either (error . show) id . parse parseAll ""

lookupRule :: [(Int, Rule)] -> Int -> Rule
lookupRule xs x = maybe (error "Rule does not exist") id (lookup x xs)

matchParser :: [(Int, Rule)] -> Int -> Parser String
matchParser rules idx = eval idx (lookupRule rules idx)
  where
    eval :: Int -> Rule -> Parser String
    eval _n (RuleChar c) = string [c]
    eval n (Rules as) =
      if elem n as
      then fmap (concat . concat) $ many1 (traverse (matchParser rules) (takeWhile (/= n) as))
      else fmap concat $ traverse (matchParser rules) as
    eval n (Or a b) = try (eval n a) <|> eval n b

matchZero :: [(Int, Rule)] -> String -> Bool
matchZero rules
  = either (const False) (const True)
  . parse (matchParser rules 0 >> char 'x') ""
  . (<> ['x'])

matchingMessages :: [(Int, Rule)] -> [String] -> Int
matchingMessages rules = length . filter (matchZero rules)

updateRules :: [(Int, Rule)] -> [(Int, Rule)]
updateRules rules =
    (8, Or (Rules [42]) (Rules [42, 8]))
  : (11, Or (Rules [42, 31]) (Rules [42, 11, 31]))
  : filter (\(x,_) -> x /= 8 || x /= 11) rules

test :: String
test = unlines
  [ "0: 4 1 5"
  , "1: 2 3 | 3 2"
  , "2: 4 4 | 5 5"
  , "3: 4 5 | 5 4"
  , "4: \"a\""
  , "5: \"b\""
  , ""
  , "ababbb"
  , "bababa"
  , "abbbab"
  , "aaabbb"
  , "aaaabbb"
  ]

test2 :: String
test2 = unlines
  [ "42: 9 14 | 10 1"
  , "9: 14 27 | 1 26"
  , "10: 23 14 | 28 1"
  , "1: \"a\""
  , "11: 42 31"
  , "5: 1 14 | 15 1"
  , "19: 14 1 | 14 14"
  , "12: 24 14 | 19 1"
  , "16: 15 1 | 14 14"
  , "31: 14 17 | 1 13"
  , "6: 14 14 | 1 14"
  , "2: 1 24 | 14 4"
  , "0: 8 11"
  , "13: 14 3 | 1 12"
  , "15: 1 | 14"
  , "17: 14 2 | 1 7"
  , "23: 25 1 | 22 14"
  , "28: 16 1"
  , "4: 1 1"
  , "20: 14 14 | 1 15"
  , "3: 5 14 | 16 1"
  , "27: 1 6 | 14 18"
  , "14: \"b\""
  , "21: 14 1 | 1 14"
  , "25: 1 1 | 1 14"
  , "22: 14 14"
  , "8: 42"
  , "26: 14 22 | 1 20"
  , "18: 15 15"
  , "7: 14 5 | 1 21"
  , "24: 14 1"
  , ""
  , "abbbbbabbbaaaababbaabbbbabababbbabbbbbbabaaaa"
  , "bbabbbbaabaabba"
  , "babbbbaabbbbbabbbbbbaabaaabaaa"
  , "aaabbbbbbaaaabaababaabababbabaaabbababababaaa"
  , "bbbbbbbaaaabbbbaaabbabaaa"
  , "bbbababbbbaaaaaaaabbababaaababaabab"
  , "ababaaaaaabaaab"
  , "ababaaaaabbbaba"
  , "baabbaaaabbaaaababbaababb"
  , "abbbbabbbbaaaababbbbbbaaaababb"
  , "aaaaabbaabaaaaababaa"
  , "aaaabbaaaabbaaa"
  , "aaaabbaabbaaaaaaabbbabbbaaabbaabaaa"
  , "babaaabbbaaabaababbaabababaaab"
  , "aabbbbbaabbbaaaaaabbbbbababaaaaabbaaabba"
  ]

test3 :: String
test3 = unlines
  [ "0: 1 | 1 0"
  , "1: \"a\""
  , ""
  , "aaaaa"
  , "aaaab"
  ]

main :: IO ()
main = do
  (rules, messages) <- fmap runParser . readFile =<< getDataFileName "inputs/day19"
  let (testRules, testMessages) = runParser test
  putStrLn "--- Test ---"
  print (matchZero testRules "ababbb")
  print (matchZero testRules "abbbab")
  print (matchZero testRules "bababa")
  print (matchZero testRules "aaabbb")
  print (matchZero testRules "aaaabbb")
  print (uncurry matchingMessages (runParser test))
  putStrLn "--- Test 2 ---"
  let (testRules, testMessages) = runParser test2
  putStrLn "Should be True:"
  print (matchZero (updateRules testRules) "bbabbbbaabaabba")
  print (matchZero (updateRules testRules) "babbbbaabbbbbabbbbbbaabaaabaaa")
  print (matchZero (updateRules testRules) "aaabbbbbbaaaabaababaabababbabaaabbababababaaa")
  print (matchZero (updateRules testRules) "bbbbbbbaaaabbbbaaabbabaaa")
  print (matchZero (updateRules testRules) "bbbababbbbaaaaaaaabbababaaababaabab")
  print (matchZero (updateRules testRules) "ababaaaaaabaaab")
  print (matchZero (updateRules testRules) "ababaaaaabbbaba")
  print (matchZero (updateRules testRules) "baabbaaaabbaaaababbaababb")
  print (matchZero (updateRules testRules) "abbbbabbbbaaaababbbbbbaaaababb")
  print (matchZero (updateRules testRules) "aaaaabbaabaaaaababaa")
  print (matchZero (updateRules testRules) "aaaabbaabbaaaaaaabbbabbbaaabbaabaaa")
  print (matchZero (updateRules testRules) "aabbbbbaabbbaaaaaabbbbbababaaaaabbaaabba")
  putStrLn "Should be False:"
  print (matchZero (updateRules testRules) "abbbbbabbbaaaababbaabbbbabababbbabbbbbbabaaaa")
  print (matchZero (updateRules testRules) "aaaabbaaaabbaaa")
  print (matchZero (updateRules testRules) "babaaabbbaaabaababbaabababaaab")
  print (matchingMessages testRules testMessages)
  print (matchingMessages (updateRules testRules) testMessages)
  putStrLn "--- Test 3 ---"
  let (testRules, testMessages) = runParser test3
  print testRules
  print testMessages
  mapM_ print (map (matchZero testRules) testMessages)
  print (uncurry matchingMessages (runParser test3))
  putStrLn "--- Real ---"
  print (matchingMessages rules messages)
  print (matchingMessages (updateRules rules) messages)
