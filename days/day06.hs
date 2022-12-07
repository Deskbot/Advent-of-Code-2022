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

poop n file = map (\i -> take n $ drop i file) [0 ..]
    & filterIndexed (not . hasMatch)
    & (+) n
  where
    hasMatch :: Eq a => [a] -> Bool
    hasMatch arr = any (\elem -> count elem arr > 1) arr

    count :: Eq a => a -> [a] -> Int
    count elem arr = foldl (\tot e -> tot + if e == elem then 1 else 0) 0 arr

filterIndexed :: (a -> Bool) -> [a] -> Int
filterIndexed f arr = head [ i | (elem, i) <- zip arr [0..], f elem]

part1 = poop 4
part2 = poop 14
