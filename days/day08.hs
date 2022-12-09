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
  file <- readFile "inputs/day08.txt"
  () <- print (part1 testInput)
  () <- print (part2 testInput)
--   () <- print (part1 file)
--   () <- print (part2 file)
  return ()

type Forest = (Int, Int, [[Char]])
type Tree = (Int, Point)
type Point = (Int, Int)

newtype Grid a = Grid (Int, Int, [(a, Point)])

getCell :: Int -> Int -> Grid a -> (a, Point)
getCell x y (Grid (_, _, plane)) = filter (\(_,p) -> p == (x,y)) plane !! 0

getRow :: Int -> Grid a -> [(a, Point)]
getRow x (Grid (_, _, plane)) = filter (\(_, (x',_)) -> x==x') plane
    & sortBy (\(_, (x1,_)) (_,(x2,_)) -> compare x1 x2)

getCol :: Int -> Grid a -> [(a, Point)]
getCol y (Grid (_, _, plane)) = filter (\(_, (_, y')) -> y == y') plane
    & sortBy (\(_, (_,y1)) (_,(_,y2)) -> compare y1 y2)

getDirections x y grid = (up,down,left,right)
    where
        up = reverse $ take x $ getCol y grid
        down = drop (x+1) $ getCol y grid
        left = reverse $ take y $ getRow x grid
        right = drop (y+1) $ getRow y grid

gridMap :: ((a, Point)->b) -> Grid a -> Grid b
gridMap f grid = Grid (rowLen, colLen, map mapCell plane)
    where
        Grid (rowLen, colLen, plane) = grid
        mapCell (val, p) = (f (val, p), p)

toArr (Grid (_,_,plane)) = map fst plane

parseFile :: String -> Forest
parseFile file = (rows, cols, lines)
    where
        lines = splitOn "\n" file
        cols = length (head lines)
        rows = length lines

parseFile2 :: String -> Grid Int
parseFile2 file = Grid (rows, cols, trees)
  where
    lines = splitOn "\n" file
    sizes = lines <&> map (\c -> read (c:"") :: Int)
    trees = combinations [0..rows] [0..cols]
        <&> (\(x,y) -> (sizes !! x !! y, (x,y)))
    cols = length (head lines)
    rows = length lines

getVisible :: [Tree] -> [Tree]
getVisible [] = []
getVisible arr = foldl keep [] arr
    where
        keep :: [Tree] -> Tree -> [Tree]
        keep visibles elem  = if (fst elem) > (biggest tallestVisibleSoFar)
            then elem:visibles
            else visibles

            where
                tallestVisibleSoFar = map fst visibles

biggest = foldl max (-1)

treesInAllDirections :: Forest -> [[Tree]]
treesInAllDirections grid = (allRows) ++ (fmap reverse allRows) ++ (allCols) ++ (fmap reverse allCols)
    where
        (rows, cols, plane) = grid

        getRow row = map (\col -> (getTreeHeight row col, (row, col))) [1 .. cols]
        getCol col = map (\row -> (getTreeHeight row col, (row, col))) [1 .. rows]

        allRows = map getRow [1 .. rows]
        allCols = map getCol [1 .. cols]

        getTreeHeight row col = read ((plane !! (row-1) !! (col-1)) : "") :: Int

combinations :: [a] -> [b] -> [(a,b)]
combinations arr1 arr2 = concatMap (\elem1 -> map (\elem2 -> (elem1, elem2)) arr2) arr1

part1 file = parseFile file
    & treesInAllDirections
    <&> getVisible
    & foldl1 (++)
    & nub -- unique
    & length

part2 file = parseFile2 file
    & (\grid -> gridMap (\(height, (x,y)) -> getDirections x y grid) grid)
    & gridMap (\((up,down,left,right), _) -> length up * length down * length left * length right)
    & toArr
    & biggest
    -- &
    -- <&> length
    -- & biggest

testInput = "30373\n\
            \25512\n\
            \65332\n\
            \33549\n\
            \35390"
