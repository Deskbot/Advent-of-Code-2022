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

newtype Grid a = Grid (Int, Int, [[(a, Point)]])

getCell x y (_, _, plane) = plane !! x !! y
getRow x (_, _, plane) = plane !! x
getCol y (_, _, plane) = map (!! y) plane
getDirections x y grid = (up,down,left,right)
    where
        up = reverse $ take x $ getCol y grid
        down = drop (x+1) $ getCol y grid
        left = reverse $ take y $ getRow x grid
        right = drop (y+1) $ getRow y grid

gridMap :: ((a, Point)->b) -> Grid a -> Grid b
gridMap f grid = Grid (rowLen, colLen, map mapRow plane)
    where
        Grid (rowLen, colLen, plane) = grid
        mapRow row = map mapCell row
        mapCell (val, p) = (f (val, p), p)

parseFile :: String -> Forest
parseFile file = (rows, cols, lines)
    where
        lines = splitOn "\n" file
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

treePlane :: Forest -> [[[Tree]]]
treePlane grid = map directions coords
    where
        (rows, cols, plane) = grid
        coords = combinations [1 .. rows] [1 .. cols]

        directions (row, col) = (reverse left) : (right) : (reverse up) : down : []
            where
                (left, right) = splitAt col (getRow row)
                (up, down) = splitAt row (getCol col)

        getRow row = map (\col -> (getTreeHeight row col, (row, col))) [1 .. cols]
        getCol col = map (\row -> (getTreeHeight row col, (row, col))) [1 .. rows]
        getTreeHeight row col = read ((plane !! (row -1) !! (col -1)) : "") :: Int

combinations :: [a] -> [b] -> [(a,b)]
combinations arr1 arr2 = concatMap (\elem1 -> map (\elem2 -> (elem1, elem2)) arr2) arr1

part1 file = parseFile file
    & treesInAllDirections
    <&> getVisible
    & foldl1 (++)
    & nub -- unique
    & length

part2 file = parseFile file
    & treePlane
    -- &
    -- <&> length
    -- & biggest

testInput = "30373\n\
            \25512\n\
            \65332\n\
            \33549\n\
            \35390"
