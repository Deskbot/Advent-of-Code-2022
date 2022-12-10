module Main where

import Data.Either
import Data.Function
import Data.Functor
import Data.List
import Data.List.Split
import Data.Sort
import Debug.Trace
import System.IO
import Text.ParserCombinators.Parsec hiding (Line)
import Text.ParserCombinators.Parsec.Number

import Modules.MyUtil (fromRightButWorking, foldlKeepHistory)

main = do
  file <- readFile "inputs/day10.txt"
  () <- print (part1 file)
  () <- print (part2 file)
  return ()

part1 file = parseFile file
  & generatePcStateOverTime
  & (\history -> map (atTime history) [20, 60, 100, 140, 180])
  & sum

part2 file = ""

data Op = Noop | Addx Int
type Pc = Int

parseFile :: String -> [Op]
parseFile file = splitOn "\n" file <&> parseLine
  where
    parseLine :: String -> Op
    parseLine line = fromRightButWorking $ parse lineParser "poop" line

    lineParser = noopParser <|> addxParser
    noopParser = string "noop" >> return Noop
    addxParser =
      string "addx "
        >> Addx <$> int

generatePcStateOverTime :: [Op] -> [(Int, Pc)]
generatePcStateOverTime = foldlKeepHistory tick (initialTime, initialPc)
  where
    initialPc = 1
    initialTime = 1

tick :: (Int, Pc) -> Op -> (Int, Pc)
tick (time, pc) op = case op of
  Noop -> (time + 1, pc)
  Addx val -> (time + 2, pc + val)

atTime :: [(Int, Pc)] -> Int -> Pc
atTime history t = snd $ last $ filter (\(time,_) -> time < t) history
