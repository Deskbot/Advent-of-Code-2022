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

import Modules.MyUtil (fromRightButWorking, foldlKeepHistory, mapi, batches, joinBetween)
import Data.Array.Base (mapIndices)
import Control.Monad (join)

main = do
  file <- readFile "inputs/day10.txt"
  -- () <- print (part1 test1)
  () <- print (part1 file)
  () <- print (part2 file)
  return ()

part1 file = parseFile file
  & generatePcStateOverTime
  & (\history -> map (signalStrength history) [20, 60, 100, 140, 180, 220])
  & sum

part2 file = parseFile file
  & (\ops -> generatePcStateAtEachTime (length ops) $ generatePcStateOverTime ops)
  <&> spriteMap
  & drawSpriteOverTime

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
atTime history t = snd $ last $ filter (\(time,_) -> time <= t) history

signalStrength history time = time * atTime history time

generatePcStateAtEachTime :: Int -> [(Int, Pc)] -> [(Int, Pc)]
generatePcStateAtEachTime until history = map (\t -> (t, atTime history t)) [1 .. until]

spriteMap :: (Int, Pc) -> [Char]
spriteMap (time, x) = map f [1..40]
  where
    f i = if abs (x - i) <= 1
      then '#'
      else '.'

drawSpriteOverTime :: [String] -> String
drawSpriteOverTime spriteMapsOverTime = batches 40 (drop 20 spriteMapsOverTime)
  <&> toString
  <&> joinBetween '\n'
  & concat

  where
    toString :: [[Char]] -> String
    toString spriteMapsOverTime = []

      where
        res = foldl f "" arr
        arr = zip [0 .. (length spriteMapsOverTime - 1)] spriteMapsOverTime

        f :: String -> (Int, [Char]) -> String
        f acc (t, sm) = mapi (\i sm -> sm !! i) (traceShowId spriteMapsOverTime)

test1 = "noop\n\
        \addx 3\n\
        \addx -5"