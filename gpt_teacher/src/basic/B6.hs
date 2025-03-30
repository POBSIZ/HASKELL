module B6 (b6Print) where

-- 1️⃣ sumAll: 리스트의 모든 숫자를 더하는 함수 (foldl 사용)
-- 	•	예: sumAll [1,2,3,4] → 10
sumAll :: [Int] -> Int
sumAll = sum

-- 2️⃣ productAll: 리스트의 모든 숫자를 곱하는 함수 (foldl 사용)
-- 	•	예: productAll [2,3,4] → 24
productAll :: [Int] -> Int
productAll = product

-- 3️⃣ reverseList: 리스트를 뒤집는 함수 (foldl 사용)
-- 	•	예: reverseList [1,2,3] → [3,2,1]
reverseList :: [Int] -> [Int]
reverseList = foldl (flip (:)) []

-- 4️⃣ zipSum: 두 개의 리스트를 받아 각 요소를 더한 리스트 반환 (zipWith 사용)
-- 	•	예: zipSum [1,2,3] [4,5,6] → [5,7,9]
zipSum :: [Int] -> [Int] -> [Int]
zipSum = zipWith (+)

b6Print :: IO ()
b6Print = do
  print (sumAll [1, 2, 3, 4])
  print (productAll [2, 3, 4])
  print (reverseList [1, 2, 3])
  print (zipSum [1, 2, 3] [4, 5, 6])
