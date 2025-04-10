{-# LANGUAGE TemplateHaskell #-}

module Lens_2 (l2) where

import Control.Lens
import Data.Char (toUpper)

{-
좋습니다! 😄
이제 Lens 중급 주제 ①: Traversal 에 대해 배워봅시다.
Traversal은 리스트, 컬렉션, 중첩 필드 등 여러 요소에 한꺼번에 접근하거나 수정할 때 사용됩니다.

✅ Traversal 이란?

“0개 이상”의 값을 대상으로 하는 Lens-like 구조입니다.

•	Lens는 하나의 필드에 접근
•	Traversal은 여러 필드에 접근

🔍 기본 연산자 정리

연산자      설명	            예시
---
(^..)     여러 값 가져오기     person ^.. address . city
(^?)      Maybe 값 가져오기   person ^? address . ix 0
(%~)	    값 변형	           obj & each %~ (*2)
(.~)	    값 설정	           obj & each .~ 0
-}

{-
✅ 1. each – 리스트, 튜플 등에 쓰이는 Traversal
✔️ each는 Foldable & Traversable한 것에 자동으로 적용됨
-}

example1 :: [Integer]
example1 = [1, 2, 3] ^.. each -- [1,2,3]

example2 :: [Integer]
example2 = [1, 2, 3] & each %~ (* 2) -- [2,4,6]

example3 :: [Integer]
example3 = [1 :: Int, 2, 3] & each .~ 0 -- [0,0,0]

{- ✅ 2. Traversal + 레코드 조합 예시 -}

data Student = Student {_grades :: [Int]} deriving (Show)

makeLenses ''Student

s1 :: Student
s1 = Student [80, 90, 70]

-- 모든 점수 5점씩 올리기
example4 :: Student
example4 = s1 & grades . each %~ (+ 5)

-- 결과: Student [85, 95, 75]

{-
✅ 3. ix – 인덱싱으로 특정 요소 수정 (0-based)
📌 ix는 Maybe 기반 → 실패할 수 있음
-}

example5 :: [Integer]
example5 = [10, 20, 30] & ix 1 .~ 99 -- [10,99,30]

example6 :: Maybe Integer
example6 = [10, 20, 30] ^? ix 2 -- Just 30

example7 :: Maybe Integer
example7 = [10, 20, 30] ^? ix 5 -- Nothing

{-
✍️ 실습 과제

아래 문제를 직접 구현해보세요!

-- 1. 모든 멤버 이름을 대문자로 바꾸세요 → "ALICE", "BOB", ...
-- 2. 두 번째 멤버를 "Bobby"로 바꾸세요
-- 3. 첫 번째 멤버 이름만 꺼내보세요 (Maybe)

힌트:
•	. each %~ ...
•	. ix 1 .~ ...
•	. ix 0 ^?
-}

data Team = Team {_members :: [String]} deriving (Show)

makeLenses ''Team

t1 :: Team
t1 = Team ["Alice", "Bob", "Charlie"]

task1 :: Team
task1 = t1 & members . each %~ map toUpper

task2 :: Team
task2 = t1 & members . ix 1 .~ "Bobby"

task3 :: Maybe [Char]
task3 = t1 ^? members . ix 0

l2 :: IO ()
l2 = do
  print $ example1
  print $ example2
  print $ example3
  print $ example4
  print $ example5
  print $ example6
  print $ example7

  print $ task1
  print $ task2
  print $ task3