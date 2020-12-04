module Main where

import Paths_aoc2020
import Data.List.Split
import Data.Monoid
import Data.Char
import Data.Bifunctor

test :: String
test = unlines
  [ "pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980"
  , "hcl:#623a2f"
  , ""
  , "eyr:2029 ecl:blu cid:129 byr:1989"
  , "iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm"
  , ""
  , "hcl:#888785"
  , "hgt:164cm byr:2001 iyr:2015 cid:88"
  , "pid:545766238 ecl:hzl"
  , "eyr:2022"
  , ""
  , "iyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719"
  , ""
  , "eyr:1972 cid:100"
  , "hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926"
  , ""
  , "iyr:2019"
  , "hcl:#602927 eyr:1967 hgt:170cm"
  , "ecl:grn pid:012533040 byr:1946"
  , ""
  , "hcl:dab227 iyr:2012"
  , "ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277"
  , ""
  , "hgt:59cm ecl:zzz"
  , "eyr:2038 hcl:74454a iyr:2023"
  , "pid:3556412378 byr:2007"
  ]

parseField :: String -> (String, String)
parseField = second tail . break (== ':')

passports :: String -> [[(String, String)]]
passports = map (map parseField . words) . splitOn "\n\n"

countValid :: [[(String, String)]] -> Int
countValid = getSum . foldMap valid
  where
    valid :: [(String, String)] -> Sum Int
    valid input = maybe (Sum 0) (const $ Sum 1) $ do
      byr <- lookup "byr" input
      iyr <- lookup "iyr" input
      eyr <- lookup "eyr" input
      hgt <- lookup "hgt" input
      hcl <- lookup "hcl" input
      ecl <- lookup "ecl" input
      pid <- lookup "pid" input
      pure (byr, iyr, eyr, hgt, hcl, ecl, pid)

parseBirthYear :: String -> Maybe Int
parseBirthYear str =
  let n = read str
  in if n >= 1920 && n <= 2002 then Just n else Nothing

parseIssueYear :: String -> Maybe Int
parseIssueYear str =
  let n = read str
  in if n >= 2010 && n <= 2020 then Just n else Nothing

parseExpirationYear :: String -> Maybe Int
parseExpirationYear str =
  let n = read str
  in if n >= 2020 && n <= 2030 then Just n else Nothing

parseHeight :: String -> Maybe Int
parseHeight str =
  let
    (numStr, unit) = break (not . isDigit) str
    num = read numStr
  in
    case unit of
      "in" -> if num >= 59 && num <= 76 then Just num else Nothing
      "cm" -> if num >= 150 && num <= 193 then Just num else Nothing
      _ -> Nothing

parseHairColor :: String -> Maybe String
parseHairColor ('#':str) =
  if length (filter (\c -> isDigit c || elem c "abcdef") str) == 6
  then Just str
  else Nothing
parseHairColor _ = Nothing

parseEyeColor :: String -> Maybe String
parseEyeColor str =
  if elem str ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
  then Just str
  else Nothing

parsePassportId :: String -> Maybe Int
parsePassportId str =
  if length str == 9
  then Just (read str)
  else Nothing

countValidStrict :: [[(String, String)]] -> Int
countValidStrict = getSum . foldMap valid
  where
    valid :: [(String, String)] -> Sum Int
    valid input = maybe (Sum 0) (const $ Sum 1) $ do
      byr <- parseBirthYear =<< lookup "byr" input
      iyr <- parseIssueYear =<< lookup "iyr" input
      eyr <- parseExpirationYear =<< lookup "eyr" input
      hgt <- parseHeight =<< lookup "hgt" input
      hcl <- parseHairColor =<< lookup "hcl" input
      ecl <- parseEyeColor =<< lookup "ecl" input
      pid <- parsePassportId =<< lookup "pid" input
      pure (byr, iyr, eyr, hgt, hcl, ecl, pid)

main :: IO ()
main = do
  pass <- fmap passports . readFile =<< getDataFileName "inputs/day4"
  print (countValid pass)
  print (countValidStrict pass)
