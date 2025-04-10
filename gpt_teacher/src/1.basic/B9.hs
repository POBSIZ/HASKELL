module B9 (b9Print) where

import Data.Map as Map

-- 1️⃣ splitBy: 특정 구분자 기준으로 리스트를 나누는 함수 (foldr 활용)
-- 	•	예: splitBy ',' "hello,world,hs" → ["hello", "world", "hs"]
splitBy :: Char -> String -> [String]
splitBy sep = Prelude.foldr f [[]]
  where
    f _ [] = []
    f c (x : xs)
      | c == sep = [] : x : xs
      | otherwise = (c : x) : xs

-- 2️⃣ mostFrequent: 리스트에서 가장 많이 등장한 요소 반환 (frequencyMap 활용)
-- 	•	예: mostFrequent "banana" → 'a'
frequencyMap :: (Ord a) => [a] -> [(a, Int)]
frequencyMap xs = Map.toList (Prelude.foldl updateCount Map.empty xs)
  where
    updateCount m c = Map.insertWith (+) c 1 m

mostFrequent :: [Char] -> Char
mostFrequent "" = ' '
mostFrequent xs =
  fst $
    Prelude.foldl
      (\(key, value) (x, count) -> if count > value then (x, count) else (key, value))
      (' ', 0)
      (frequencyMap xs)

-- 3️⃣ fastFib: 피보나치 수열을 효율적으로 계산하는 함수 (tail recursion 활용)
-- 	•	예: fastFib 10 → 55
fastFib :: Integer -> Integer
fastFib n = fib n (0, 1)
  where
    fib c (a, b)
      | c == 0 = a
      | otherwise = fib (c - 1) (b, a + b)

-- 4️⃣ flatten: 중첩된 리스트를 평탄화하는 함수 (foldr 활용)
-- 	•	예: flatten [[1,2],[3,4],[],[5]] → [1,2,3,4,5]
flatten :: [[Integer]] -> [Integer]
flatten xs = Prelude.foldr (.) id (Prelude.map (++) xs) []

b9Print :: IO ()
b9Print = do
  print $ splitBy ',' "hello,world,hs"
  print $ mostFrequent "banana"
  print $ fastFib 10
  print $ flatten [[1, 2], [3, 4], [], [5]]