module B7 (b7Print) where

-- 1️⃣ countEvens: 리스트에서 짝수 개수를 세는 함수 (filter & length 활용)
-- 	•	예: countEvens [1,2,3,4,6] → 3
countEvens :: [Int] -> Int
countEvens = length . filter even

-- 2️⃣ doubleAll: 리스트의 모든 숫자를 두 배로 만드는 함수 (map 사용)
-- 	•	예: doubleAll [1,2,3] → [2,4,6]
doubleAll :: [Int] -> [Int]
doubleAll = map (* 2)

-- 3️⃣ sumOfSquares: 리스트의 모든 요소의 제곱의 합을 구하는 함수 (map & foldl 사용)
-- 	•	예: sumOfSquares [1,2,3] → 1² + 2² + 3² = 14
sumOfSquares :: [Integer] -> Integer
sumOfSquares = sum . map (^ (2 :: Integer))

-- 4️⃣ maxInList: 리스트에서 가장 큰 값을 반환하는 함수 (foldl1 사용)
-- 	•	예: maxInList [3,7,2,9,5] → 9
-- 	•	힌트: foldl1 max
-- ! foldl1 = 빈 배열을 인자로 받을 경우 에러가 발생함
maxInList :: [Int] -> Int
maxInList = maximum

b7Print :: IO ()
b7Print = do
  print (countEvens [1, 2, 3, 4, 6])
  print (doubleAll [1, 2, 3])
  print (sumOfSquares [1, 2, 3])
  print (maxInList [3, 7, 2, 9, 5])