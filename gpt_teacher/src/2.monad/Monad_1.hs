module Monad_1 (m1) where

-- 1. What is Monad

-- Monad :: String
-- Monad = 함수형 프로그래밍에서 연산을 순차적으로 연결할 수 있게 해주는 일종의 컨테이너이자 추상화된 계산 규칙
-- Monad = 함수형 프로그래밍에서 연산을 정의하고 추상화하여 합성 가능한 연산을 만들 수 있도록 하는 개념
-- Monad = 연산의 순서를 제어하고 부작용을 다루기 위해 컨텍스트 안에서 값을 감싸고, 연속적인 연산을 안전하게 연결할 수 있게 해주는 추상화이다.

-- 1-1. Functor
-- 컨테이너 안의 값을 변형하는 인터페이스
functor :: IO ()
functor = do
  putStrLn $ "-- Functor"
  print $ fmap (+ (1 :: Integer)) (Just 3) -- Just 4
  print $ fmap (+ (1 :: Integer)) Nothing -- Nothing
  putStrLn $ ""

-- 1-2. Applicative
-- 컨테이너 안에 함수가 들어있을 때 사용
foo :: (Num a) => a -> a -> a -> a
foo x y z = x + y + z

applicative :: IO ()
applicative = do
  putStrLn $ "-- Applicative"
  print $ Just (+ (1 :: Int)) <*> Just 2
  print $ pure (+) <*> Just (1 :: Int) <*> Just 2
  print $ pure (foo :: Int -> Int -> Int -> Int) <*> Just 1 <*> Just 2 <*> Just 3
  putStrLn $ ""

-- 1-3. Monad
-- 컨테이너 안에 값이 있는 구조에서 다음 계산으로 연결해주는 인터페이스
monad :: IO ()
monad = do
  putStrLn $ "-- Monad"
  print $ Just (3 :: Int) >>= (\x -> Just (x + 1)) -- Just 4
  print $ Nothing >>= (\x -> Just (x + (1 :: Int))) -- Nothing
  putStrLn $ ""

-- ! Monad Type Class Define

-- Applicative 값을 모나드 컨텍스트로 바꾼다
-- class (Applicative m) => Monad m where

-- Bind 모나드 값에서 순수 값을 뽑아내어 함수를 적용한 후 다시 모나드 컨텍스트로 감싼 후 반환한다.
-- (>>=) :: m a -> (a -> m b) -> m b

-- Return
-- 순수한 값을 모나드 컨텍스트로 감싼다
-- return :: a -> m a

-- ! Quiz
-- 1️⃣ Just 5 >>= (\x -> Just (x * 2)) 의 결과는?
-- A : Just 10

-- 2️⃣ Nothing >>= (\x -> Just (x * 2)) 의 결과는?
-- A : Nothing

-- 3️⃣ return 3 >>= (\x -> return (x + 10)) 은 어떤 결과를 줄까요?
-- A : Just 13

-- 4️⃣ fmap (+1) Nothing 은 어떤 결과일까요?
-- A : Just 1 = X
-- A : Nothing = O

m1 :: IO ()
m1 = do
  functor
  applicative
  monad