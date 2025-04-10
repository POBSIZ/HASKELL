{-# LANGUAGE InstanceSigs #-}

module My_Monad_11 (m11) where

{-
  ✅ 목표: 내가 직접 Monad 타입과 인스턴스를 정의하기

  📦 예제: 간단한 로깅 모나드 만들기

  🔸 우리가 만들 기능
    •	어떤 계산이든 결과와 로그 메시지를 함께 담을 수 있는 Logger a 타입
    •	return은 값만 주고 로그는 없음
    •	>>=는 이전 로그 + 새로운 로그를 합쳐야 함
-}

{-
  ✅ 1. 타입 정의
  ✔️ 값 a와 로그 [String]을 담는 컨테이너
-}
newtype Logger a = Logger {runLogger :: (a, [String])}

{-
  ✅ 2. Functor 인스턴스
  •	값에 함수 적용, 로그는 그대로 유지
-}
instance Functor Logger where
  fmap :: (a -> b) -> Logger a -> Logger b
  fmap f (Logger (x, _log)) = Logger (f x, _log)

{-
  ✅ 3. Applicative 인스턴스
	•	로그를 ++로 이어 붙입니다
-}
instance Applicative Logger where
  pure :: a -> Logger a
  pure x = Logger (x, [])
  (<*>) :: Logger (a -> b) -> Logger a -> Logger b
  Logger (f, log1) <*> Logger (x, log2) = Logger (f x, log1 ++ log2)

{-
  ✅ 4. Monad 인스턴스
	•	>>=는 값을 꺼내서 함수에 넣고, 로그를 누적합니다
-}
instance Monad Logger where
  return :: a -> Logger a
  return = pure
  (>>=) :: Logger a -> (a -> Logger b) -> Logger b
  Logger (x, log1) >>= f =
    let Logger (y, log2) = f x
     in Logger (y, log1 ++ log2)

{-
  ✅ 5. 로그를 남기는 함수
-}
logMsg :: String -> Logger ()
logMsg msg = Logger ((), [msg])

{-
  ✅ 예제 사용
-}
calc :: Logger Int
calc = do
  logMsg "Start calculation"
  let x = 3 + 5
  logMsg $ "Calculated x = " ++ show x
  let y = x * 2
  logMsg $ "Calculated y = " ++ show y
  return y

{-
  🎯 실습 과제

  직접 Logger 모나드를 아래와 같이 만들어보세요:
  1.	newtype Logger a = Logger ...
  2.	Functor, Applicative, Monad 인스턴스
  3.	logMsg :: String -> Logger ()
  4.	예제: calcLogger :: Logger Int
    calcLogger = do
      logMsg "Doing 2 + 2"
      let a = 2 + 2
      logMsg "Multiplying by 3"
      return (a * 3)
-}

newtype MyLogger a = MyLogger {runMyLogger :: (a, [String])}

instance Functor MyLogger where
  fmap :: (a -> b) -> MyLogger a -> MyLogger b
  fmap f (MyLogger (a, logs)) = MyLogger (f a, logs)

instance Applicative MyLogger where
  pure :: a -> MyLogger a
  pure x = MyLogger (x, [])
  (<*>) :: MyLogger (a -> b) -> MyLogger a -> MyLogger b
  MyLogger (f, log1) <*> MyLogger (x, log2) = MyLogger (f x, log1 ++ log2)

instance Monad MyLogger where
  return :: a -> MyLogger a
  return = pure
  (>>=) :: MyLogger a -> (a -> MyLogger b) -> MyLogger b
  MyLogger (x, log1) >>= f =
    let MyLogger (y, log2) = f x
     in MyLogger (y, log1 ++ log2)

myLogMsg :: String -> MyLogger ()
myLogMsg msg = MyLogger ((), [msg])

calcLogger :: MyLogger Integer
calcLogger = do
  myLogMsg "Doing 2 + 2"
  let a = 2 + 2
  myLogMsg "Multiplying by 3"
  return (a * 3)

m11 :: IO ()
m11 = do
  print $ runLogger calc
  print $ runMyLogger calcLogger
