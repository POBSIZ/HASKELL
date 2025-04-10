{-
✅ Lens 입문 – 목표 요약

중첩된 레코드 구조에서 값을 읽고, 수정하고, 부분만 교체하는 작업을
함수형답게, 깔끔하게, 안전하게 할 수 있도록 도와줍니다.

👀 전통적인 필드 수정의 문제

data Address = Address {city :: String, zipCode :: Int}
data Person = Person {name :: String, address :: Address}

도시 이름을 바꾸고 싶다면?
setCity :: Person -> Person
setCity p = p {address = (address p) {city = "Seoul"}}

😖 중첩이 많아질수록… 괄호와 필드 수정을 반복해야 하죠.

🎯 Lens 사용 시

person & addressL . cityL .~ "Seoul"

📌 (&)는 값 → 함수 적용의 방향을 바꾸는 연산자
📌 (.~)는 값을 “대체”하는 lens 연산자
📌 addressL, cityL은 각각 필드를 가리키는 Lens

✅ Lens 기초 문법

연산자	의미
---
(^.)	값 꺼내기 (Getter)
(.~)	값 설정 (Setter)
(%~)	함수 적용 (Modifier)
(&)	  파이프 연산자 (x & f == f x)
-}
{-# LANGUAGE TemplateHaskell #-}

module Lens_1 (l1) where

import Control.Lens (makeLenses, (%~), (&), (.~), (^.))
import Data.Char (toUpper)

{- ✨ 실습 준비 -}

-- 1. 데이터 정의
data Address = Address
  { _city :: String,
    _zipCode :: Int
  }
  deriving (Show)

data Person = Person
  { _name :: String,
    _address :: Address
  }
  deriving (Show)

-- makeLenses는 _name, _city 등으로부터 name, city 등의 Lens를 자동 생성합니다.
makeLenses ''Address
makeLenses ''Person

{- 🧠 실습 예제 -}

p1 :: Person
p1 = Person "Alice" (Address "Busan" 12345)

-- 도시 이름 가져오기
example1 :: String
example1 = p1 ^. address . city -- "Busan"

-- 우편번호를 99999로 바꾸기
example2 :: Person
example2 = p1 & address . zipCode .~ 99999

-- 도시명을 대문자로 바꾸기
example3 :: Person
example3 = p1 & address . city %~ map toUpper

{- ✍️ 실습 과제 -}
-- 1. p1의 이름을 "Bob"으로 바꾸세요
practice1 :: Person
practice1 = p1 & name .~ "Bob"

-- 2. 주소 전체를 다음으로 교체하세요: Address "Seoul" 54321
practice2 :: Person
practice2 = p1 & address .~ Address "Seoul" 54321

-- 3. 이름과 도시를 동시에 바꾸는 함수 `renameAndMove`를 작성하세요:
renameAndMove :: String -> String -> Person -> Person
renameAndMove pName pCity p = p & name .~ pName & address . city .~ pCity

l1 :: IO ()
l1 = do
  print example1
  print example2
  print example3
  print practice1
  print practice2
  print $ renameAndMove "Test" "Busan" p1