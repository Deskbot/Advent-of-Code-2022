import System.IO
import Data.Function
import Data.Functor
import Data.List
import Data.List.Split
import Data.Sort
import Debug.Trace

main = do
    file <- readFile "inputs/day03.txt"
    () <- putStrLn $ show $ part1 file
    putStrLn $ show $ part2 file

type Bag = (String, String)

parseFile :: String -> [Bag]
parseFile file = splitOn "\n" file
    <&> compartments

    where
        compartments str = (first, second)
            where
                compartmentSize = (length str) `div` 2
                first = take compartmentSize str
                second = drop compartmentSize str

commonItem :: Bag -> Char
commonItem (comp1, comp2) = (intersect comp1 comp2) !! 0

priority :: Char -> Int
priority c = case elemIndex c priorities of
    Just p -> p + 1 -- indexes start at 0
    Nothing -> error "no priority found for char"
    where
        priorities = ['a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z','A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z']

batches :: Int -> [t] -> [[t]]
batches n [] = []
batches n arr = (take n arr):(batches n (drop n arr))

commonItemBetweenElves :: [Bag] -> Char
commonItemBetweenElves bags = bags
    <&> join
    & foldl1 intersect
    & head

    where
        join (arr1, arr2) = union arr1 arr2

part1 file = parseFile file
    <&> commonItem
    <&> priority
    & sum

part2 file = parseFile file
    & batches 3
    <&> commonItemBetweenElves
    <&> priority
    & sum
