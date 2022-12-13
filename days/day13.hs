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
  file <- readFile "inputs/day13.txt"
  () <- print (part1 file)
  () <- print (part2 file)
  return ()

data Packet = Seq [Packet] | Atom Int

parseFile :: String -> Packet
parseFile file = Grid (rows, cols, cells)
  where
    pairsOfStrings = splitOn "\n\n" file
      <&> splitOn "\n"

parsePacket :: String -> Packet
parsePacket str = ""
  where
    packetParser = number | seqParser
    seqParser = do
      _ <- char '['
      n1 <- optionMaybe packetParser
      ns <- many (char ',' >> packetParser)
      _ <- char ']'
      return Seq [n1 : ns]

part1 file = ""
  where
    grid = parseFile file
    startPos = find ((==) 'S') grid
    endPos = find ((==) 'E') grid
