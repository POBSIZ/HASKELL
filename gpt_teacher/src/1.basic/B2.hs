module B2 (b2Print) where

-- 1. isEven: 주어진 숫자가 짝수인지 판별하는 함수 (힌트: `mod` 연산자 사용)
isEven :: Int -> Bool
isEven = even

-- 2. greet: 주어진 이름을 받아서 "Hello, [이름]!"을 반환하는 함수
greet :: String -> String
greet a = "Hello, " ++ a ++ "!"

-- 3. hypotenuse: 직각삼각형의 빗변 길이를 구하는 함수 (힌트: `sqrt(a^2 + b^2)`)
hypotenuse :: Double -> Double -> Double
hypotenuse a b = sqrt (a ^ 2 + b ^ 2)

b2Print :: IO ()
b2Print = do
  print (isEven 3)
  print (greet "user")
  print (hypotenuse 2 2)