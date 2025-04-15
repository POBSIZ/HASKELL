{-# LANGUAGE TemplateHaskell #-}

module Prism_1 (p1, p1_2) where

import Control.Lens

{-
Prism은 ADT (Algebraic Data Types), 특히 분기 구조를 다룰 때 사용하는 도구입니다.

⸻

✅ Prism이란?

“값을 꺼내거나 만들어주는 선택적 구조”
Maybe, Either, Bool, 사용자 정의 Sum 타입 등에서 유용하게 사용됩니다.

⸻

🧠 Lens vs Prism vs Optional 비교

구조         설명                   예시
---
Lens        항상 존재하는 필드 접근    person ^. name
Optional    존재할 수도 있는 필드     list ^? ix 1
Prism       분기 중 하나에 접근       Left "err" ^? _Left

⸻

✅ Prism 기본 예제

🔹 Maybe와 Prism

Just 5 ^? _Just         -- Just 5
Nothing ^? _Just        -- Nothing

_Just # 10              -- Just 10 (Prism으로 "구성"하기)

🔹 Either와 Prism

Right "ok" ^? _Right    -- Just "ok"
Left "err" ^? _Right    -- Nothing

_Right # "value"        -- Right "value"
_Left  # "oops"         -- Left "oops"

⸻
-}
{-
✅ 사용자 정의 ADT에 Prism 붙이기
1. ADT 정의
-}

data LoginResult
  = Success String
  | Failure Int
  deriving (Show)

makePrisms ''LoginResult

-- 2 . 사용 예시

r1 :: LoginResult
r1 = Success "token123"

r2 :: LoginResult
r2 = Failure 401

p1 :: IO ()
p1 = do
  putStrLn $ show (r1 ^? _Success) -- Just "token123"
  putStrLn $ show (r2 ^? _Success) -- Nothing
  putStrLn $ show (r2 ^? _Failure) -- Just 401
  putStrLn $ show (_Success # "newToken") -- Success "token123"

{-
✍️ 실습 과제

다음 Prism 문제를 직접 풀어보세요!

-- 1. r1에서 상태 코드(Int)를 꺼내세요 (`^?`)
-- A : rr1 ^? _Ok

-- 2. r2가 _Ok인지 확인해보세요 (`^?`)
-- A : rr2 ^? _Ok

-- 3. r3의 주소를 "http" → "https"로 바꾸세요 (`%~`)
-- A : rr3 & _Redirect .~ "https://example.com"

-- 4. Int를 Ok로 감싸는 표현식을 Prism으로 만들어보세요 (`#`)
-- A : _Ok # 200
-}

data Response
  = Ok Int
  | NotFound
  | Redirect String
  deriving (Show)

makePrisms ''Response

rr1 :: Response
rr1 = Ok 200

rr2 :: Response
rr2 = NotFound

rr3 :: Response
rr3 = Redirect "https://example.com"

replaceHttpsWithHttp :: String -> String
replaceHttpsWithHttp url = "http" ++ drop 5 url

p1_2 :: IO ()
p1_2 = do
  print $ rr1 ^? _Ok
  print $ rr2 ^? _Ok
  print $ rr3 & _Redirect %~ replaceHttpsWithHttp
  print $ _Ok # 200