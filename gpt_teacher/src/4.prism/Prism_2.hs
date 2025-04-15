module Prism_2 (p2) where

import Control.Lens
import Data.Char (isDigit)
import Text.Read (readMaybe)

{-
이제 Lens 고급 주제 ④: Custom Prism 직접 만들기에 도전해봅시다.

이건 Lens/Prism의 진짜 구조와 작동 원리를 이해하는 단계입니다.
makePrisms 없이 직접 프리즘을 정의하면,
더 정밀한 제어와 커스터마이징이 가능해집니다.

⸻

✅ 프리즘 직접 만들기 – 핵심 개념

프리즘은 두 가지 기능을 제공합니다:
	1.	값을 “꺼내기” – 패턴 매칭 (^?)
	2.	값을 “만들기” – 생성자처럼 사용 (#)

이 두 기능을 묶어서 정의합니다.

⸻

✅ prism 함수 시그니처

prism :: (b -> t) -> (s -> Either t a) -> Prism s t a b

파라미터	의미
review   (b -> t)	값 만들기
matcher  (s -> Either t a)	값 꺼내기 (패턴 매칭)

⸻

🧪 실습 예제: Custom String ↔ Int Prism 만들기

목표:
•	문자열이 모두 숫자일 때만 → Int로 바꾸기
•	Int를 → 문자열로 만들기

-}
{-
✅ 1. 프리즘 정의
-}

stringInt :: Prism' String Int
stringInt = prism show convert
  where
    convert s =
      if all isDigit s
        then maybe (Left s) Right (readMaybe s)
        else Left s

{-
✍️ 실습 과제

아래 프리즘을 직접 만들어보세요!

🎯 목표: Char ↔ Bool Prism
•	'1' → True, '0' → False
•	True → '1', False → '0'
•	그 외는 매칭 실패

시그니처:

bitChar :: Prism' Char Bool

예시 결과:

'1' ^? bitChar     -- Just True
'0' ^? bitChar     -- Just False
'X' ^? bitChar     -- Nothing

bitChar # True     -- '1'
bitChar # False    -- '0'
-}

bitChar :: Prism' Char Bool
bitChar =
  prism reviewer matcher
  where
    reviewer :: Bool -> Char
    reviewer b
      | b == True = '1'
      | otherwise = '0'
    matcher :: Char -> Either Char Bool
    matcher b
      | b == '1' = Right True
      | b == '0' = Right False
      | otherwise = Left b

p2 :: IO ()
p2 = do
  print $ '1' ^? bitChar -- Just True
  print $ '0' ^? bitChar -- Just False
  print $ 'X' ^? bitChar -- Nothing
  print $ bitChar # True -- '1'
  print $ bitChar # False -- '0'