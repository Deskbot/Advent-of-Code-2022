{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
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
import Text.ParserCombinators.Parsec hiding (Line)
import Text.ParserCombinators.Parsec.Number
import Modules.Grid

main = do
  file <- readFile "inputs/day12.txt"
  () <- print (part1 file)
  () <- print (part2 file)
  return ()

parseFile :: String -> Grid Char
parseFile file = Grid (rows, cols, cells)
  where
    arr2d = splitOn "\n" file
    rows = arr2d & first & length
    cols = arr2d & length
    cells = combinations [0..rows-1] [0..cols-1]
      & map (\(x,y) -> (arr2d !! x !! y, (x,y)))

part1 file = ""
  where
    grid = parseFile file
    startPos = find (\c -> c == 'S') grid
    endPos = find (\c -> c == 'E') grid