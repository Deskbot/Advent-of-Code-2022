import Data.Function
import Data.Functor
import Data.List.Split
import Debug.Trace
import System.IO
import Text.ParserCombinators.Parsec hiding (Line)
import Text.ParserCombinators.Parsec.Number
import Modules.MyUtil

main = do
  file <- readFile "inputs/day13.txt"
  () <- print (part1 file)
  -- () <- print (part2 file)
  return ()

data Packet = Seq [Packet] | Atom Int deriving (Show)

parseFile :: String -> [(Packet,Packet)]
parseFile file = splitOn "\n" file
  & filter (/= "")
  <&> parsePacket
  & batches 2
  <&> (\batch -> (batch !! 0, batch !! 1))

parsePacket :: String -> Packet
parsePacket str = case parse packetParser "poop" str of
  Left err -> error $ show err
  Right move -> move

  where
    packetParser = atomParser <|> seqParser
    atomParser = do
      n <- int
      return (Atom n)
    seqParser = do
      _ <- char '['
      arr <- numsParser
      _ <- char ']'
      return (Seq arr)
    numsParser = do
      n1 <- optionMaybe packetParser
      ns <- many (char ',' >> packetParser)
      return (case n1 of
        Just n -> n:ns
        Nothing -> [])

part1 file = parseFile file

