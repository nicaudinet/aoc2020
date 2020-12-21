module Main where

import Paths_aoc2020

import Data.List
import Data.List.Split
import qualified Data.Map as M
import qualified Data.Set as S

parseFood :: String -> (S.Set String, [String])
parseFood str =
  let [foods, allergens] = splitOn " (contains " str
  in (S.fromList (words foods), map init (words allergens))

parse :: String -> [(S.Set String, [String])]
parse = map parseFood . lines

type AllergenMap = M.Map String (S.Set String)

allergenMap :: [(S.Set String,[String])] -> AllergenMap
allergenMap = foldr fn M.empty
  where
    fn :: (S.Set String, [String]) -> AllergenMap -> AllergenMap
    fn (foods, allergens) m = foldr (M.alter (gn foods)) m allergens

    gn :: S.Set String -> Maybe (S.Set String) -> Maybe (S.Set String)
    gn foods Nothing = Just foods
    gn foods (Just foods') = Just (S.intersection foods foods')

nonAllergenFoods :: [(S.Set String, [String])] -> Int
nonAllergenFoods xs = sum (map count foods)
  where
    allergenFoods :: S.Set String
    allergenFoods = S.unions . M.elems $ allergenMap xs

    foods :: [String]
    foods = S.toList . (S.\\ allergenFoods) . S.unions $ map fst xs
  
    count :: String -> Int
    count str = length . filter (== str) . concat $ map (S.toList . fst) xs

-- lets do it by hand!
hand :: [(String, String)]
hand =
  [ ("shellfish", "xkb") -- < made a typo here. Serves me right I guess
  , ("eggs", "sqdsxhb")
  , ("sesame", "dnlsjr")
  , ("fish", "dgvqv")
  , ("wheat", "rsvlb")
  , ("nuts", "csnfnl")
  , ("soy", "lkdg")
  , ("dairy", "pbhthx")
  ]

allergens :: AllergenMap -> [(String, String)]
allergens = go []
  where
    go :: [(String, String)] -> AllergenMap -> [(String, String)]
    go as m
      | S.null (S.unions (M.elems m)) = as
      | otherwise =
          let newAs =
                M.foldrWithKey
                  (\k v a -> if S.size v == 1 then (k, head (S.elems v)):a else a)
                  as
                  m
              newM = foldr (\str -> M.map (S.\\ S.singleton str)) m (map snd newAs)
          in go newAs newM

printAllergens :: [(String, String)] -> String
printAllergens = intercalate "," . map snd . sortOn fst

main :: IO ()
main = do
  foods <- fmap parse . readFile =<< getDataFileName "inputs/day21"
  mapM_ print (M.toList $ allergenMap foods)
  print (nonAllergenFoods foods)
  putStrLn (printAllergens hand)
  putStrLn (printAllergens (allergens (allergenMap foods)))
