module B10 (b10Print) where

import Data.List (transpose, uncons)

-- 1️⃣ groupByN: 리스트를 N개씩 묶어서 튜플 리스트로 반환
-- 	•	예: groupByN 3 [1,2,3,4,5,6,7,8,9] → [(1,2,3), (4,5,6), (7,8,9)]
-- 	•	splitBy를 확장하여 구현 가능
-- groupByN :: Integer -> [Integer] -> [(Integer)]
groupByN :: Int -> [a] -> [[a]]
groupByN n xs
  | length xs < n = []
  | otherwise = take n xs : groupByN n (drop n xs)

-- 2️⃣ lazyFib: 무한 피보나치 리스트 생성 (lazy evaluation 활용)
-- 	•	예: take 10 lazyFib → [0,1,1,2,3,5,8,13,21,34]
-- 	•	무한 리스트를 활용하여 한 번 계산된 값을 재사용
lazyFib :: [Integer]
lazyFib = 0 : 1 : zipWith (+) lazyFib (drop 1 lazyFib)

-- 3️⃣ quickSort: 퀵 정렬 알고리즘을 Haskell로 구현 (list comprehension 활용)
-- 	•	예: quickSort [3,1,4,1,5,9] → [1,1,3,4,5,9]
-- 	•	filter와 list comprehension을 활용 가능
quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
quickSort (p : xs) =
  let small = quickSort [x | x <- xs, x <= p]
      big = quickSort [x | x <- xs, x > p]
   in small ++ [p] ++ big

quickSort2 :: (Ord a) => [a] -> [a]
quickSort2 [] = []
quickSort2 (p : xs) =
  quickSort2 (filter (<= p) xs) ++ [p] ++ quickSort2 (filter (> p) xs)

-- 4️⃣ transposeMatrix: 행렬을 전치(Transpose)하는 함수
-- 	•	예: transposeMatrix [[1,2,3], [4,5,6], [7,8,9]] → [[1,4,7], [2,5,8], [3,6,9]]
-- 	•	힌트: zip을 활용 가능
transposeMatrix :: [[a]] -> [[a]]
transposeMatrix [] = []
transposeMatrix ([] : _) = []
transposeMatrix xs =
  case mapM uncons xs of
    Nothing -> []
    Just headsAndTails -> map fst headsAndTails : transposeMatrix (map snd headsAndTails)

transposeMatrix2 :: [[a]] -> [[a]]
transposeMatrix2 = transpose

b10Print :: IO ()
b10Print = do
  print $ groupByN 3 [(1 :: Integer), 2, 3, 4, 5, 6, 7, 8, 9]
  print $ take 10 lazyFib
  print $ quickSort [(3 :: Integer), 1, 4, 1, 5, 9]
  print $ quickSort2 [(3 :: Integer), 1, 4, 1, 5, 9]
  print $ transposeMatrix [[(1 :: Integer), 2, 3], [4, 5, 6], [7, 8, 9]]
  print $ transposeMatrix2 [[(1 :: Integer), 2, 3], [4, 5, 6], [7, 8, 9]]