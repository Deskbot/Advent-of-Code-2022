import System.IO
import Data.Functor
import Data.List.Split
import Debug.Trace

main = do
    file <- readFile "inputs/day01.txt"
    () <- putStrLn $ show $ part1 file
    putStrLn $ show $ part2 file

makeElvesFromInput :: String -> [[Int]]
makeElvesFromInput input = splitOn "\n\n" input
    <&> splitOn "\n"
    <&> noEmptyCalories
    <&> fmap (\calorieStr -> read calorieStr :: Int)

    where
    noEmptyCalories = filter (\calories -> calories /= "")

part1 file = foldl1 max elfTotCalories
    where
        elfTotCalories = makeElvesFromInput file <&> sum

part2 file = sum bestElves
    where
        bestElves = maxn elfTotCalories 3
        elfTotCalories = makeElvesFromInput file <&> sum

maxn :: [Int] -> Int -> [Int]
maxn [] _ = []
maxn arr count = if length arr <= count
    then traceShowId arr
    else if head arr > foldl1 min maxnOfRemaining
        then (head arr):(maxn remaining (count-1))
        else maxnOfRemaining

    where
        remaining = tail arr
        maxnOfRemaining = maxn remaining count
