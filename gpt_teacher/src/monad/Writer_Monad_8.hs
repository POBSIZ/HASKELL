module Writer_Monad_8 (m8) where

{-
  ✅ Writer Monad – 로그를 함께 남기자

  “계산을 하면서 로그, 메시지, 기록을 남길 수 없을까?”
  👉 Writer Monad는 결과와 함께 로그를 축적하는 데 쓰입니다.

  📦 개념
  newtype Writer w a = Writer { runWriter :: (a, w) }
  •	a: 계산 결과
	•	w: 로그 (문자열, 리스트 등 Monoid이어야 함)

  📌 로그는 모노이드(+)로 누적됨
  → 문자열 (String), 리스트 ([String]), 숫자 (Sum Int) 등

  ✅ 기본 함수

  tell – 로그 추가
  tell :: Monoid w => w -> Writer w ()

  writer – (결과, 로그)로 직접 생성
  writer :: (a, w) -> Writer w a

  runWriter – 계산 실행하여 결과와 로그 확인
-}

import Control.Monad.Writer (MonadWriter (tell), Writer, runWriter)

-- ✅ 예제: 로그 남기며 계산하기
-- runWriter (addWithLog 3 4)
-- 결과: (7, ["Added 3 and 4 = 7"])
addWithLog :: Int -> Int -> Writer [String] Int
addWithLog x y = do
  let result = x + y
  tell ["Added " ++ show x ++ " and " ++ show y ++ " = " ++ show result]
  return result

-- ✅ 예제: 여러 로그 누적
-- runWriter calc
-- 결과: (6, ["Added 1 and 2 = 3", "Added 3 and 3 = 6"])
calc :: Writer [String] Int
calc = do
  a <- addWithLog 1 2
  b <- addWithLog a 3
  return b

-- ✨ 실습 과제
-- 아래의 multiplyWithLog와 calcMult를 직접 만들어보세요:
-- runWriter calcMult
-- 결과: (24, ["Multiplied 2 and 3 = 6", "Multiplied 6 and 4 = 24"])

-- x와 y를 곱하고, 로그를 남기는 함수
multiplyWithLog :: Int -> Int -> Writer [String] Int
multiplyWithLog x y = do
  let result = x * y
  tell ["Multiplied " ++ show x ++ " and " ++ show y ++ " = " ++ show result]
  return result

-- 2 * 3, 그 결과를 4와 다시 곱하는 로그 누적 함수
calcMult :: Writer [String] Int
calcMult = do
  a <- multiplyWithLog 2 3
  multiplyWithLog a 4

m8 :: IO ()
m8 = do
  print $ runWriter (addWithLog 3 4)
  print $ runWriter calc
  print $ runWriter calcMult

{-
  🔁 핵심 요약 – Writer Monad

  tell        : 로그 한 줄 추가
  Writer w a  : 로그 w와 결과 a를 함께 담는 컨테이너
  reun Writer : 실행 결과 + 로그 추출
  Monoid 제약  : 로그 타입은 반드시 Monoid여야 함 (String, [a], Sum, Product, ...)
-}