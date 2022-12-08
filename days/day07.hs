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

data Line = Cd String | Ls | Entry (String, Int) | Dir String deriving (Show)

type File = (String, Int)
type Folder = (String, [FileSystem])
data FileSystem = Folder Folder | File File deriving (Show)
type Cwd = [String]

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
            return $ Entry (name, size)
        parseFolder = do
            _ <- string "dir "
            name <- literallyAnything
            return $ Dir name

        literallyAnything = many $ satisfy (const True)

buildFileSystem :: [Line] -> FileSystem
buildFileSystem lines = Folder $ snd $ foldl applyLine ([], ("", [])) lines
    where
        applyLine :: (Cwd, Folder) -> Line -> (Cwd, Folder)
        applyLine (cwd, folder) line = case line of
            Cd ".."   -> (tail cwd, folder)
            Cd to     -> (to : cwd, (to, []))
            Ls        -> (cwd, folder)
            Entry f   -> (cwd, (folderName, (File f):children))
            Dir name  -> (cwd, (folderName, (Folder (name, [])):children))
            where
                (folderName, children) = folder


getFolders :: FileSystem -> [FileSystem]
getFolders (File _) = []
getFolders (Folder children) = map getFolders (snd children)
    & foldl1 (++)

folderSize :: Folder -> Int
folderSize (name, children) = filter isFile children
    & map toFile
    & map getFileSize
    & sum

isFile fs = case fs of
    Folder _ -> False
    File _ -> True

toFile fs = case fs of
    File f -> f
    Folder _ -> error "expected a file"

getFileSize :: File -> Int
getFileSize = snd

getSize :: FileSystem -> Int
getSize (File (_, size)) = size
getSize (Folder (name, children)) = map getSize children
    & sum

part1 file = parseFile file
    & buildFileSystem
    & traceShowId
    & getFolders
    & map getSize
    & filter (<= 100000)
    & sum

part2 file = ""
