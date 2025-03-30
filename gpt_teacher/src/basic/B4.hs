module B4 (b4Print) where

-- 고차 함수 (High Order Functions - HOF)

-- 1️⃣ applyThrice: 함수를 세 번 적용하는 함수
-- 	•	예: applyThrice double 2 → 16
-- 	•	힌트: applyThrice f x = f (f (f x))
applyThrice :: (a -> a) -> a -> a
applyThrice f = f . f . f

-- 2️⃣ squareList: 리스트의 모든 요소를 제곱하는 함수
-- 	•	예: squareList [1,2,3] → [1,4,9]
-- 	•	힌트: map 사용
squareList :: [Int] -> [Int]
squareList = map (^ (2 :: Int))

-- 3️⃣ onlyOdd: 리스트에서 홀수만 필터링하는 함수
-- 	•	예: onlyOdd [1,2,3,4,5] → [1,3,5]
-- 	•	힌트: filter 사용
onlyOdd :: [Int] -> [Int]
onlyOdd = filter odd

b4Print :: IO ()
b4Print = do
  print (applyThrice (* 2) (2 :: Integer))
  print (squareList [1, 2, 3])
  print (onlyOdd [1, 2, 3, 4, 5])