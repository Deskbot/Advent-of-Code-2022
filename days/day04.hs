import System.IO
import Data.Function
import Data.Functor
import Data.List
import Data.List.Split
import Data.Sort
import Debug.Trace

main = do
    file <- readFile "inputs/day04.txt"
    () <- putStrLn $ show $ part1 file
    putStrLn $ show $ part2 file

type Range = (Int,Int)

parseFile :: String -> [(Range,Range)]
parseFile file = splitOn "\n" file
    <&> parseLine

parseLine :: String -> (Range,Range)
parseLine str = splitOn "," str
    <&> parseRange
    & \(group1:group2:[]) -> (group1, group2)

parseRange :: String -> Range
parseRange str = splitOn "-" str
    <&> (\str -> read str :: Int)
    & \(group1:group2:[]) -> (group1, group2)

hasSubRange :: (Range,Range) -> Bool
hasSubRange ((start1, end1), (start2, end2)) = (start1 >= start2 && end1 <= end2)
    || (start1 <= start2 && end1 >= end2)

hasOverlap :: (Range,Range) -> Bool
hasOverlap (range1, range2) = isBetween start1 range2
    || isBetween end1   range2
    || isBetween start2 range1
    || isBetween end2   range1
    where
        (start1, end1) = range1
        (start2, end2) = range2
        isBetween n (start, end) = n >= start && n <= end

part1 file = parseFile file
    & filter hasSubRange
    & length

part2 file = parseFile file
    & filter hasOverlap
    & length
