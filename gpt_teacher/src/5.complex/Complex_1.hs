{-# LANGUAGE TemplateHaskell #-}

module Complex_1 (c1) where

import Control.Lens
import Control.Monad.State

{-
이제 Lens의 강력함을 상태(State)와 결합해서 실전에서 어떻게 활용할 수 있는지를 배워봅시다.

⸻

🎯 목표: Lens + State Monad 통합 실습

“복잡한 상태 값을 깔끔하게 수정하고 읽을 수 있는 함수형 방식”

우리는 Lens를 이용해 State Monad 내부의 필드를
•	쉽게 읽고 (use, preuse)
•	수정하고 (.=, %=)
•	조합해서 관리할 수 있습니다.

-}

-- 🧱 예제 구조: RPG 캐릭터 상태
data Character = Character
  { _hp :: Int,
    _mp :: Int,
    _name :: String
  }
  deriving (Show)

makeLenses ''Character

{-
✅ State Monad와 Lens 연동 – 핵심 연산자

연산자          설명
use field     현재 상태에서 해당 필드 값을 가져오기 (State 모나드 안에서)
field .= val  필드를 새로운 값으로 설정
field %= f    필드에 함수 적용 (기존 값 변형)
zoom lens     부분 상태만 포커싱해서 서브 State 실행
-}

-- 🧪 예제: 캐릭터 행동 정의

-- HP 10 감소
takeDamage :: State Character ()
takeDamage = hp %= (\h -> h - 10)

-- 이름 변경
rename :: String -> State Character ()
rename newName = name .= newName

-- 행동 조합
gameTurn :: State Character ()
gameTurn = do
  takeDamage
  rename "BraveHero"

runGame :: Character -> Character
runGame = execState gameTurn

{-
✍️ 실습 과제

직접 아래를 작성해보세요!
1.	초기 캐릭터: 이름 "Alice", HP 100, MP 50
2.	castSpell 함수: MP를 20 감소
3.	fullRestore 함수: HP와 MP를 모두 100으로 회복
4.	magicTurn: castSpell + takeDamage + 이름 "Mage" 로 바꾸기
5.	runMagic으로 실행해 최종 결과 출력
-}

initCharacter :: Character
initCharacter = Character 100 50 "Alice"

castSpell :: State Character ()
castSpell = mp -= 20

fullRestore :: State Character ()
fullRestore = do
  mp .= 100
  hp .= 100

magicTurn :: State Character ()
magicTurn = do
  castSpell
  takeDamage
  rename "Mage"

runMagic :: Character -> Character
runMagic = execState magicTurn

c1 :: IO ()
c1 = do
  print $ runGame initCharacter
  print $ runMagic initCharacter