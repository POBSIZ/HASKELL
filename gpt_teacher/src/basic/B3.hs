module B3 (b3Print) where

-- 1️⃣ factorial: 정수를 입력받아 해당 숫자의 팩토리얼을 계산하는 함수
-- 	•	예: factorial 5 → 120
-- 	•	힌트: factorial 0 = 1, factorial n = n * factorial (n-1)
factorial :: Integer -> Integer
factorial x
  | x <= 0 = 1
  | otherwise = x * factorial (x - 1)

-- 2️⃣ fib: 피보나치 수열을 계산하는 함수
-- 	•	예: fib 5 → 5 (0, 1, 1, 2, 3, 5, …)
-- 	•	힌트: fib 0 = 0, fib 1 = 1, fib n = fib (n-1) + fib (n-2)
fib :: Integer -> Integer
fib x
  | x <= 1 = x
  | otherwise = fib (x - 1) + fib (x - 2)

-- 3️⃣ describeNum: 입력된 숫자에 따라 다른 문자열을 반환하는 함수
-- •	0이면 "Zero"
-- •	1이면 "One"
-- •	그 외의 숫자는 "Other"
describeNum :: Integer -> String
describeNum x
  | x == 0 = "Zero"
  | x == 1 = "One"
  | otherwise = "Other"

fibFast :: Int -> Int
fibFast n = fibHelper n (0, 1)
  where
    fibHelper 0 (a, _) = a
    fibHelper n (a, b) = fibHelper (n - 1) (b, a + b)

b3Print :: IO ()
b3Print = do
  print (factorial 5)
  print (fib 5)
  print (describeNum 0)
  print (describeNum 1)
  print (describeNum 2)
  print (fibFast 5)