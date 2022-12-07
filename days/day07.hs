{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
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
import Text.ParserCombinators.Parsec hiding (Line)
import Text.ParserCombinators.Parsec.Number

main = do
  file <- readFile "inputs/day07.txt"
  () <- print (part1 file)
  () <- print (part2 file)
  return ()

data Line = Cd String | Ls | File (Int, String) | Folder (String) deriving (Show)

parseFile :: String -> [Line]
parseFile file = splitOn "\n" file
    <&> parseLine

parseLine :: String -> Line
parseLine str = case result of
    Left err -> error $ show err
    Right line -> line

    where
        result = parse parser "poop" str

        parser = try parseCd <|> parseLs <|> parseFile <|> parseFolder
        parseCd = do
            _ <- string "$ cd "
            file <- literallyAnything
            return $ Cd file
        parseLs = do
            _ <- string "$ ls"
            return Ls
        parseFile = do
            size <- int
            _ <- string " "
            name <- literallyAnything
            return $ File (size, name)
        parseFolder = do
            _ <- string "dir "
            name <- literallyAnything
            return $ Folder name

        literallyAnything = many $ satisfy (const True)

part1 file = parseFile file

part2 file = ""
