import System.IO
import Data.Function
import Data.Functor
import Data.List.Split
import Data.Sort
import Debug.Trace

main = do
    file <- readFile "inputs/day02.txt"
    () <- putStrLn $ show $ test1
    () <- putStrLn $ show $ part1 file
    putStrLn $ show $ part2 file

data Move = Rock | Paper | Scissors deriving (Eq, Show)
type Game = (Move, Move)
data Result = Win | Draw | Loss

parseFile :: String -> [Game]
parseFile file = splitOn "\n" file
    <&> splitOn " "
    <&> \([myMove, theirMove]) -> (parseMove theirMove, parseMove myMove) -- yes I'm reversing the order
    where
        parseMove "A" = Rock
        parseMove "B" = Paper
        parseMove "C" = Scissors
        parseMove "X" = Rock
        parseMove "Y" = Paper
        parseMove "Z" = Scissors
        parseMove _ = error "no such move"

score :: Game -> Int
score game = resultValue (result game) + shapeValue myMove
    where
        resultValue Win = 6
        resultValue Draw = 3
        resultValue Loss = 0

        shapeValue Rock = 1
        shapeValue Paper = 2
        shapeValue Scissors = 3

        (myMove, theirMove) = game

result :: Game -> Result
result (Rock, Paper) = Loss
result (Rock, Scissors) = Win
result (Scissors, Rock) = Loss
result (Scissors, Paper) = Win
result (Paper, Scissors) = Loss
result (Paper, Rock) = Win
result _ = Draw

part1 file = parseFile file
    <&> score
    & sum

part2 file = ""

test1 = "expected: " ++ show expected ++ " actual: " ++ show (part1 file)
    where
        expected = 15
        file = "A Y\n\
               \B X\n\
               \C Z"
