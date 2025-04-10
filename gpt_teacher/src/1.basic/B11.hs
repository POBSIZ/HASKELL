module B11 (b11Print) where

import Control.Parallel.Strategies (parList, parMap, rdeepseq, rseq, using)

-- 1️⃣ parMapFib: 여러 개의 피보나치 숫자를 병렬 계산 (parMap 활용)
-- 	•	예: parMapFib [30, 32, 34] → [832040, 2178309, 5702887]
-- 	•	힌트: Control.Parallel.Strategies.parMap 활용
fib :: Integer -> Integer
fib n = nfib n (0, 1)
  where
    nfib 0 (a, _) = a
    nfib x (a, b) = nfib (x - 1) (b, a + b)

parMapFib :: [Integer] -> [Integer]
parMapFib = parMap rdeepseq fib

-- 2️⃣ mergeSort: 머지 정렬을 Haskell로 구현 (splitAt 활용)
-- 	•	예: mergeSort [5,3,8,1,2] → [1,2,3,5,8]
-- 	•	퀵 정렬보다 안정적인 정렬 알고리즘
mergeSort :: [Integer] -> [Integer]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = merge (mergeSort left) (mergeSort right)
  where
    mid = length xs `div` 2
    (left, right) = splitAt mid xs
    merge mxs [] = mxs
    merge [] mxs = mxs
    merge (x1 : xs1) (x2 : xs2)
      | x1 <= x2 = x1 : merge xs1 (x2 : xs2)
      | otherwise = x2 : merge (x1 : xs1) xs2

-- 3️⃣ concurrentSum: 여러 개의 숫자를 병렬로 더하는 함수 (parList 활용)
-- 	•	힌트: Control.Parallel.Strategies 활용
concurrentSum :: [Integer] -> Integer
concurrentSum xs = sum (xs `using` parList rseq)

-- 4️⃣ monadExample: Maybe 모나드를 활용하여 안전한 나눗셈 함수 작성
-- 	•	예: safeDiv 10 2 → Just 5
-- 	•	safeDiv 10 0 → Nothing
safeDiv :: Integer -> Integer -> Maybe Integer
safeDiv _ 0 = Nothing
safeDiv a b = do
  let result = div a b
  return result

b11Print :: IO ()
b11Print = do
  print $ parMapFib [30, 32, 34]
  print $ mergeSort [5, 3, 8, 1, 2]
  print $ concurrentSum [1 .. 1000000]
  print $ safeDiv 10 2
  print $ safeDiv 10 0