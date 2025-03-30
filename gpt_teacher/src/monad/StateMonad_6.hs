module M6 (m6) where

-- 6. State / Reader / Writer Monad
{-
  State  : 상태를 전달하는 모나드 (가변 변수 없이 상태 관리!)
  Reader : 앍기 전용 환경 전달 (예: 설정, 전역 값 등)
  Writer : 로깅/출력 누적 (예: 디버깅 메시지, 결과 트레이싱 등)
-}

-- 6-1. State Monad – 상태를 함수형으로 다루는 방법

{-
  📦 기본 개념

  | newtype State s a = State { runState :: s -> (a, s) }
  •	s: 상태(state) 타입
	•	a: 결과값 타입
	•	runState: 상태를 받아서 (값, 새로운 상태)를 반환하는 함수

  예시
  | State Int String  -- 상태는 Int, 결과는 String
-}

import Control.Monad.State (MonadState (get, put), State, modify, runState)

increment :: State Int Int
increment = do
  n <- get
  put (n + 1)
  return n

twoSteps :: State Int Int
twoSteps = do
  a <- increment -- 상태: 0 → 1
  b <- increment -- 상태: 1 → 2
  return (a + b) -- a = 0, b = 1 → 결과: 1

-- 세 번 increment 하고, 마지막 상태를 리턴하는 함수
threeIncr :: State Int Int
threeIncr = do
  _ <- increment
  _ <- increment
  x <- increment
  return x

-- 🧠 이해도 테스트

-- 1️⃣ runState increment 5 의 결과는?
-- A : (5, 6)

-- 2️⃣ runState twoSteps 10 의 결과는?
-- A : (21, 12)

-- 3️⃣ 위 threeIncr를 실행하면 (?, ?) 이 나올까요?
-- A : runState threeIncr 0 -> (0, 3) X
-- A : runState threeIncr 0 -> (2, 3) O

-- Stack Simulation
type Stack = [Int]

pop :: State Stack (Maybe Int)
pop = do
  stack <- get
  case stack of
    (x : xs) -> do
      put xs
      return $ Just x
    [] -> return Nothing

push :: Int -> State Stack ()
push n = modify (n :)

stackProgram :: State Stack (Maybe Int)
stackProgram = do
  push 10
  push 20
  _ <- pop
  pop

stackDemo :: IO ()
stackDemo = do print $ runState stackProgram []

m6 :: IO ()
m6 = do
  print $ runState increment 5
  print $ runState twoSteps 10
  print $ runState threeIncr 0
  stackDemo
