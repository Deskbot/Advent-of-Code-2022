module MyUtil (fromRightButWorking, foldlKeepHistory) where

fromRightButWorking e = case e of
  Left err -> error $ show err
  Right move -> move

foldlKeepHistory :: (a -> b -> a) -> a -> [b] -> [a]
foldlKeepHistory f init arr = reverse $ foldl f' [init] arr
  where
    f' arr b = f (head arr) b : arr
