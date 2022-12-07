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

main = do
  file <- readFile "inputs/day06.txt"
  () <- print (part1 file)
  () <- print (part2 file)
  return ()

part1 file = map (\i -> take 4 $ drop i file) [0 ..]
    & filter (not.hasMatch)
    & head

    where
        hasMatch :: Eq a => [a] -> Bool
        hasMatch arr = any (\elem -> count elem arr > 1) arr

        count :: Eq a => a -> [a] -> Int
        count elem arr = foldl (\tot e -> tot + if e == elem then 1 else 0) 0 arr



part2 file = ""
