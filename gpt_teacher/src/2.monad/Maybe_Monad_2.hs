module Maybe_Monad_2 (m2) where

-- 2. Maybe Monad – 실패 가능성 표현하기

-- * 2-1. Maybe 란?

-- data Maybe a = Nothing | Just a
-- Maybe는 성공 또는 실패(없음) 을 표현하는 대표적인 모나드

-- e.g.,

safeDiv :: Int -> Int -> Maybe Int
safeDiv _ 0 = Nothing
safeDiv a b = Just (a `div` b)

-- * 2-2. 여러 연산 연결하기 (>>=)

-- e.g.,

safeRoot :: Double -> Maybe Double
safeRoot x
  | x < 0 = Nothing
  | otherwise = Just (sqrt x)

safeHalf :: Double -> Maybe Double
safeHalf x
  | x == 0 = Nothing
  | otherwise = Just (x / 2)

-- 체인 연결
computation :: Double -> Maybe Double
computation x =
  safeRoot x >>= safeHalf >>= safeRoot

-- do 블럭으로 가독성 향상
computationDo :: Double -> Maybe Double
computationDo x = do
  r1 <- safeRoot x
  r2 <- safeHalf r1
  safeRoot r2

m2 :: IO ()
m2 = do
  putStrLn $ "-- Maybe Monad"
  print $ safeDiv 16 2
  print $ computation 16
  print $ computationDo 16
  putStrLn $ ""

-- ! Quiz
-- 1️⃣ Just 4 >>= (\x -> if x > 0 then Just (x * 2) else Nothing) → ?
-- A : Just 8

-- 2️⃣ safeHalf = \x -> if x == 0 then Nothing else Just (x / 2)
-- Just 0 >>= safeHalf → ?
-- A : Nothing

-- 3️⃣ 아래 do 블록 결과는?
-- do
--   x <- Just 10
--   y <- Just 5
--   return (x - y)
-- A : Just 5

-- 4️⃣ computation 0의 결과는 무엇일까요?
-- A : Nothing