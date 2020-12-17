module Main where

import Paths_aoc2020

import Data.List
import qualified Data.Map as M
import Data.Maybe

data Cube = Active | Inactive
  deriving (Show, Eq)

parseCube :: Char -> Cube
parseCube '.' = Inactive
parseCube '#' = Active

parse :: String -> [[Cube]]
parse = map (map parseCube) . lines

-- (x, y, z)
type Coord = (Int, Int, Int, Int)
type Space = M.Map Coord Cube 

findCube :: Space -> Coord -> Cube
findCube space = fromMaybe Inactive . flip M.lookup space

neighbors :: Coord -> Space -> [Cube]
neighbors (x, y, z, w) space = map (findCube space) neighborCoords
  where
    neighborCoords :: [Coord]
    neighborCoords = filter (/= (x,y,z,w))
      [ (a, b, c, d)
      | a <- [x-1, x, x+1]
      , b <- [y-1, y, y+1]
      , c <- [z-1, z, z+1]
      , d <- [w-1, w, w+1]
      ]

initSpace :: [[Cube]] -> Space
initSpace
  = M.fromList
  . concat
  . zipWith (\x as -> zipWith (\y a -> ((x,y,0,0), a)) [0..] as) [0..]

rule :: Cube -> [Cube] -> Cube
rule cube ns =
  let x = length (filter (== Active) ns)
  in
    if (cube == Active && x == 2) || x == 3
    then Active
    else Inactive

maxActiveCoords :: Space -> Coord
maxActiveCoords space =
  let (xs, ys, zs, ws) = unzip4 (M.keys $ M.filter (== Active) space)
  in (maximum xs, maximum ys, maximum zs, maximum ws)

minActiveCoords :: Space -> Coord
minActiveCoords space =
  let (xs, ys, zs, ws) = unzip4 (M.keys $ M.filter (== Active) space)
  in (minimum xs, minimum ys, minimum zs, minimum ws)

applyRule :: Space -> Coord -> (Coord, Cube)
applyRule space coord =
  (coord, rule (findCube space coord) (neighbors coord space))

stepV1 :: Space -> Space
stepV1 space = M.fromList (map (applyRule space) coords)
  where
    (maxX, maxY, maxZ, _) = maxActiveCoords space
    (minX, minY, minZ, _) = minActiveCoords space

    coords :: [Coord]
    coords =
      [ (a, b, c, 0)
      | a <- [minX-3 .. maxX+3]
      , b <- [minY-3 .. maxY+3]
      , c <- [minZ-3 .. maxZ+3]
      ]

stepV2 :: Space -> Space
stepV2 space = M.fromList (map (applyRule space) coords)
  where
    (maxX, maxY, maxZ, maxW) = maxActiveCoords space
    (minX, minY, minZ, minW) = minActiveCoords space

    coords :: [Coord]
    coords =
      [ (a, b, c, d)
      | a <- [minX-3 .. maxX+3]
      , b <- [minY-3 .. maxY+3]
      , c <- [minZ-3 .. maxZ+3]
      , d <- [minW-3 .. maxW+3]
      ]

run :: (Space -> Space) -> [[Cube]] -> Space
run step = (!! 6) . iterate step . initSpace

activeCubes :: Space -> Int
activeCubes = length . filter (== Active) . M.elems

main :: IO ()
main = do
  initCubes <- fmap parse . readFile =<< getDataFileName "inputs/day17"
  mapM_ print initCubes
  print (activeCubes $ run stepV1 initCubes)
  print (activeCubes $ run stepV2 initCubes)
