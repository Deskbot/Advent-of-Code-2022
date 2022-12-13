module Modules.Grid (Grid, Point) where

type Point = (Int,Int)
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
        up =    reverse $ take x     $ getCol y grid
        down =            drop (x+1) $ getCol y grid
        left =  reverse $ take y     $ getRow x grid
        right =           drop (y+1) $ getRow x grid

gridMap :: ((a, Point)->b) -> Grid a -> Grid b
gridMap f grid = Grid (rowLen, colLen, map mapCell plane)
    where
        Grid (rowLen, colLen, plane) = grid
        mapCell (val, p) = (f (val, p), p)

toArr (Grid (_,_,plane)) = map fst plane

find f grid = filter f (toArr grid) & map snd
