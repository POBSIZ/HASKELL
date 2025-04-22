{-# LANGUAGE TemplateHaskell #-}

module Lens_3 (l3) where

import Control.Lens
import Data.Map as M

{-
Optional은 조건부로 존재할 수도 있는 값을 다룰 때 사용하는 Lens 구조입니다.

⸻

✅ Optional 이란?

“있을 수도 있고 없을 수도 있는 값”에 접근/수정할 때 사용합니다.

대표적으로:
	•	Maybe 값 (Just, Nothing)
	•	Map, Vector 등에서 인덱싱
	•	ix, _Just, _Nothing, at 등이 Optional입니다.

⸻

✅ 주요 Optional 연산자

연산자   설명
---
^?    조건부 값 접근 (Maybe)
.~     값 설정
%~     함수 적용
?~     값을 새로 넣거나 제거 (Nothing으로)

⸻

✨ 예시 1: _Just로 Maybe 수정하기

Just 10 & _Just %~ (+5)     -- Just 15
Nothing & _Just %~ (+5)     -- Nothing

Just "hi" ^? _Just          -- Just "hi"
Nothing ^? _Just            -- Nothing

⸻

✨ 예시 2: ix로 안전한 인덱스 수정

[1,2,3] & ix 1 %~ (*10)     -- [1,20,3]
[1,2,3] ^? ix 5             -- Nothing

⸻

✨ 예시 3: at과 Map

import qualified Data.Map as M

m1 = M.fromList [(1,"one"), (2,"two")]

m1 ^? at 2              -- Just (Just "two")
m1 & at 2 .~ Nothing    -- 삭제됨 → Map에서 2 제거
m1 & at 3 ?~ "three"    -- 3 → "three" 삽입

⸻

✍️ 실습 과제

다음 문제를 풀어보세요!
	1.	Just 42에 8을 더해서 Just 50을 만드세요
	A : Just 42 & _Just %~ (+ 8)

	2.	Nothing을 _Just %~ (+5)로 바꾸면?
	A : Nothing

	3.	M.fromList [(1,"one"),(2,"two")] 에서 키 2를 제거하세요
	A : M.fromList [(1, "one"), (2, "two")] & at 2 .~ Nothing

	4.	같은 Map에 키 3 → "three" 를 추가하세요
	A : M.fromList [(1, "one")] & at 3 ?~ "three"

	5.	리스트 [10,20,30]에서 안전하게 2번째 값을 꺼내세요 (^?)
	A : [10, 20, 30] ^? ix 2
-}

l3 :: IO ()
l3 = do
  print ""