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
colIndexes = [0 .. 8]

parseFile :: String -> ([Stack], [Move])
parseFile file = (parseStacks (parts !! 0), parseMoves (parts !! 1))
    where
        parts = splitOn "\n\n" file

parseStacks :: String -> [Stack]
parseStacks str = colNums
    <&> (\i -> i * 4 + 1)
    <&> (\i -> fmap (\line -> (traceShowId  line) !! i) lines)
    <&> filter isLetter
    where
        lines = splitOn "\n" str
        isLetter c = elem c str

parseMoves :: String -> [Move]
parseMoves str = splitOn "\n" str
    <&> (\line -> getAllTextMatches (regex =~ line))
    <&> fmap read
    <&> toMove
    where
        regex = "move (\\d+) from (\\d+) to (\\d+)"
        toMove arr = Move {
            amount = (traceShowId arrI) !! 0,
            from = arrI !! 1,
            to = arrI !! 2
        } where
            arrI = fmap read arr

applyMove :: Move -> [Stack] -> [Stack]
applyMove Move {amount = amount, from = from, to = to} stacks = [pick i stacks | i <- colNums]
    where
        pick i stacks
          | i == from = newFrom
          | i == to = newTo
          | otherwise = stacks !! (i - 1)

        newTo = (take amount (stacks !! (from - 1))) ++ (stacks !! (to - 1))
        newFrom = drop amount (stacks !! (from - 1))

part1 file = foldl (flip applyMove) stacks moves
    <&> head
    where
        (stacks, moves) = parseFile file

part2 file = ""
