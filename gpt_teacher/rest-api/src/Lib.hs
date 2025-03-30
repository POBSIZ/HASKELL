module Lib
  ( mergeSort,
  )
where

mergeSort :: (Integral a) => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = merge (mergeSort left) $ mergeSort right
  where
    mid = length xs `div` 2
    (left, right) = splitAt mid xs
    merge [] ys = ys
    merge ys [] = ys
    merge (x1 : xs1) (x2 : xs2)
      | x1 <= x2 = x1 : merge xs1 (x2 : xs2)
      | otherwise = x2 : merge (x1 : xs1) xs2