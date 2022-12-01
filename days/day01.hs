import System.IO

main = do
    file <- readFile "inputs/day01.txt"
    putStrLn $ part1 file

part1 file = "po"