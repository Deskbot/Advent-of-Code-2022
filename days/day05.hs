import System.IO
import Data.Function
import Data.Functor
import Data.List
import Data.List.Split
import Data.Sort
import Debug.Trace
import Text.Regex.Base
import Text.Regex.Base.RegexLike
import Text.Regex.PCRE

main = do
    file <- readFile "inputs/day05.txt"
    () <- print (part1 file)
    print (part2 file)

type Stack = [Char]

data Move = Move {
    amount :: Int,
    from :: Int,
    to :: Int
} deriving (Show)

colNums = [1 .. 9]

parseFile :: String -> ([Stack], [Move])
parseFile file = (stacks, moves)
    where
        stacks = parseStacks (parts !! 0)
        moves = parseMoves (parts !! 1)
        parts = splitOn "\n\n" file

parseStacks :: String -> [Stack]
parseStacks str = colNums
    <&> (\i -> (i - 1) * 4 + 1)
    <&> (\i -> fmap (!! i) lines)
    <&> filter isLetter
    where
        lines = splitOn "\n" str
        isLetter c = elem c ['A'..'Z']

parseMoves :: String -> [Move]
parseMoves str = splitOn "\n" str
    <&> extractNums
    <&> fmap (\n -> read n :: Int)
    <&> toMove
    where
        extractNums :: String -> [String]
        extractNums str = splitOn "move " str
            <&> splitOn " from "
            & flat
            <&> splitOn " to "
            & flat
            & filter (/= "")

        toMove :: [Int] -> Move
        toMove arr = Move {
            amount = arr !! 0,
            from = arr !! 1,
            to = arr !! 2
        }

applyMove :: Move -> [Stack] -> [Stack]
applyMove Move {amount = amount, from = from, to = to} stacks = map pick colNums
    where
        pick :: Int -> Stack
        pick num
            | num == from = newFrom
            | num == to = newTo
            | otherwise = stacks !! (num - 1)

        newTo = (take amount (stacks !! (from - 1))) ++ (stacks !! (to - 1))
        newFrom = drop amount (stacks !! (from - 1))

flat = foldl1 (++)

part1 file = foldl (flip applyMove) stacks moves
    <&> head
    where
        (stacks, moves) = parseFile file

part2 file = ""
