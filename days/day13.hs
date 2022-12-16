import Data.Function
import Data.Functor
import Data.List.Split
import Debug.Trace
import System.IO
import Text.Parsec
import Modules.MyUtil
import Text.ParserCombinators.Parsec.Number

main = do
  file <- readFile "inputs/day13.txt"
  () <- print (part1 testFile)
  -- () <- print (part1 file)
  -- () <- print (part2 file)
  return ()


part1 file = parseFile file
  <&> traceShowId
  <&> rightOrder
  <&> traceShowId
  & filter (== True)
  & mapi (\i elem -> i + 1)
  & sum


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

rightOrder :: (Packet, Packet) -> Bool

rightOrder (Atom a, Atom b) = a <= b
rightOrder (Atom a, Seq b) = rightOrder (Seq [Atom a], Seq b)
rightOrder (Seq b, Atom a) = rightOrder (Seq b, Seq [Atom a])

rightOrder (Seq [], _) = True
rightOrder (Seq arr1, Seq []) = False
rightOrder (Seq arr1, Seq arr2) = if rightOrder ((head arr1), (head arr2))
  then rightOrder (Seq $ tail arr1, Seq $ tail arr2)
  else False


testFile = "[1,1,3,1,1]\n\
           \[1,1,5,1,1]\n\
           \\n\
           \[[1],[2,3,4]]\n\
           \[[1],4]\n\
           \\n\
           \[9]\n\
           \[[8,7,6]]\n\
           \\n\
           \[[4,4],4,4]\n\
           \[[4,4],4,4,4]\n\
           \\n\
           \[7,7,7,7]\n\
           \[7,7,7]\n\
           \\n\
           \[]\n\
           \[3]\n\
           \\n\
           \[[[]]]\n\
           \[[]]\n\
           \\n\
           \[1,[2,[3,[4,[5,6,7]]]],8,9]\n\
           \[1,[2,[3,[4,[5,6,0]]]],8,9]\n"
