import System.IO
import Data.Functor
import Data.List.Split
import Data.Sort
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
maxn arr count = take count $ reverse $ sort arr
