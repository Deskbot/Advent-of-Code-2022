import Data.Function
import Data.Functor
import Data.List
import Data.List.Split
import Data.Sort
import Debug.Trace
import System.IO
import Text.Regex.Base
import Text.Regex.Base.RegexLike
import Text.Regex.PCRE

main = do
  file <- readFile "inputs/day08.txt"
  () <- print (part1 file)
  () <- print (part2 file)
  return ()

type Grid = (Int, Int, [[Char]])
type Tree = (Int, Point)
type Point = (Int, Int)

parseFile :: String -> Grid
parseFile file = (rows, cols, lines)
    where
        lines = splitOn "\n" file
        cols = length (head lines)
        rows = length lines

getVisible :: [Tree] -> [Tree]
getVisible [] = []
getVisible arr = foldl keep [] arr
    where
        keep :: [Tree] -> Tree -> [Tree]
        keep visibles elem  = if (fst elem) > (biggest tallestVisibleSoFar)
            then elem:visibles
            else visibles

            where
                tallestVisibleSoFar = map fst visibles

biggest = foldl max 0

treesInAllDirections :: Grid -> [[Tree]]
treesInAllDirections grid = (allRows) ++ (reverse allRows) ++ (allCols) ++ (reverse allCols)
    where
        (rows, cols, plane) = grid
        getRow row = map (\col -> (getTreeHeight row col, (row, col))) [1 .. cols]
        getCol col = map (\row -> (getTreeHeight col col, (row, col))) [1 .. rows]
        allRows = map getRow [1 .. rows]
        allCols = map getCol [1 .. cols]
        getTreeHeight row col = read ((plane !! (row-1) !! (col-1)) : "") :: Int

part1 file = parseFile file
    & treesInAllDirections
    <&> getVisible
    & foldl1 (++)
    & nub
    & length



part2 file = ""
