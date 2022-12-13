module Modules.MyUtil (fromRightButWorking, foldlKeepHistory, mapi, batches, joinBetween, overwrite) where
import Data.ByteString (intersperse)

fromRightButWorking e = case e of
  Left err -> error $ show err
  Right move -> move

foldlKeepHistory :: (a -> b -> a) -> a -> [b] -> [a]
foldlKeepHistory f init arr = reverse $ foldl f' [init] arr
  where
    f' arr b = f (head arr) b : arr

mapi :: (Int -> a -> b) -> [a] -> [b]
mapi f arr = map f' [0..(length arr - 1)]
  where
    f' i = f i (arr !! i)

batches :: Int -> [t] -> [[t]]
batches n [] = []
batches n arr = (take n arr) : (batches n (drop n arr))

joinBetween :: a -> [a] -> [a]
joinBetween c []          = []
joinBetween c (elem : []) = [elem]
joinBetween c (el1 : el2 : arr) = el1 : c : el2 : c : joinBetween c arr

overwrite char i str = (take (i -1) str) ++ [char] ++ (drop (i) str)

combinations :: [a] -> [b] -> [(a,b)]
combinations arr1 arr2 = concatMap (\elem1 -> map (\elem2 -> (elem1, elem2)) arr2) arr1
