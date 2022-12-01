import System.IO
import Data.Functor
import Data.List.Split
import Debug.Trace

main = readFile "inputs/day01.txt"
    <&> part1
    <&> show
    >>= putStrLn


part1 file = foldl1 max elves
    where
    elves :: [Integer]
    elves = splitOn "\n\n" file
        <&> splitOn "\n"
        <&> noEmptyCalories
        <&> fmap (\calorieStr -> read calorieStr :: Integer)
        <&> sum

    noEmptyCalories = filter (\calories -> calories /= "")
