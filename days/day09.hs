import Data.Function
import Data.Functor
import Data.List
import Data.List.Split
import Data.Sort
import Debug.Trace
import System.IO
import Text.ParserCombinators.Parsec hiding (Line)
import Text.ParserCombinators.Parsec.Number
import Data.Either

main = do
  file <- readFile "inputs/day09.txt"
  () <- print (part1 file)
  () <- print (part2 file)
  return ()

part1 file = parseFile file
    & toSteps
    & computeTailHistory 2
    & nub
    & length

part2 file = parseFile file
    & toSteps
    & computeTailHistory 10
    & nub
    & length

data Direction = U | D | L | R
type Move = (Direction, Int)

type Rope = [Point]
type Point = (Int, Int)

parseFile :: String -> [Move]
parseFile file = splitOn "\n" file <&> parseLine

    where
        parseLine :: String -> Move
        parseLine line = fromRightButWorking $ parse lineParser "poop" line

        lineParser = do
            dir <- dirParser
            _ <- string " "
            dis <- int
            return (dir, dis)

        dirParser = upParser <|> downParser <|> leftParser <|> rightParser
        upParser = satisfy (== 'U') >> return U
        downParser = satisfy (== 'D') >> return D
        leftParser = satisfy (== 'L') >> return L
        rightParser = satisfy (== 'R') >> return R

fromRightButWorking e = case e of
    Left err -> error $ show err
    Right move -> move

toSteps :: [Move] -> [Direction]
toSteps moves = foldl1 (++) $ map f moves
    where
        f (dir, dis) = replicate dis dir

computeTailHistory :: Int -> [Direction] -> [Point]
computeTailHistory knots steps = snd $ foldl next (initialRope, []) steps
    where
    initialRope = replicate knots (1, 1)

    next :: (Rope, [Point]) -> Direction -> (Rope, [Point])
    next (rope, history) direction = (newRope, newHistory)
        where
        newRope = doStep rope direction
        newHistory = last newRope : history

doStep :: Rope -> Direction -> Rope
doStep rope direction = newRope
    where
        newRope = newHead : newTail

        -- very cool
        head = rope !! 0
        tail = drop 1 rope

        newHead = case direction of
            U -> (headX, headY + 1)
            D -> (headX, headY - 1)
            L -> (headX - 1, headY)
            R -> (headX + 1, headY)

            where
                (headX, headY) = head

        newTail :: Rope
        newTail = snd $ foldl next (head, []) tail
            where
                next :: (Point, Rope) -> Point -> (Point, Rope)
                next (prev, result) next = (next, result ++ [updateTail prev next])

        updateTail prev next = if tooFar prev next
            then prev
            else next
            where
                tooFar (x1,y1) (x2,y2) = abs (x1-x2) > 1 || abs (y1-y2) > 1
