{-# LANGUAGE TemplateHaskell #-}

module Todo where

import Control.Lens
import Control.Monad.State

{-
이제 당신이 익힌 Lens, State, Monad, Prism 등 Haskell의 강력한 도구들을 활용해서,
실제로 어플리케이션 구조를 설계해보는 실전 단계에 들어갑니다.

⸻

🎯 목표: 실전 앱 구조에서 Lens + State 활용하기

우리는 다음과 같은 구조의 미니 어플리케이션을 만들어볼 거예요:

💡 예시 프로젝트: To-Do 리스트 관리 시스템

✅ 할 일 목록을 추가하고, 완료하고, 수정하고, 제거하는 기능
✅ 상태를 State로 관리하고, Lens로 필드에 접근/수정
✅ 추후 IO나 Transformer, UI와 연결도 가능
-}

-- 🧱 데이터 구조 설계

-- 단일 ToDo 항목
data Todo = Todo
  { _tid :: Int,
    _content :: String,
    _done :: Bool
  }
  deriving (Show)

-- 전체 앱 상태
data AppState = AppState
  { _todos :: [Todo],
    _nextId :: Int
  }
  deriving (Show)

makeLenses ''Todo
makeLenses ''AppState

{-
✍️ 실습 과제
	1.	위 데이터 타입과 makeLenses 선언을 만들고
	2.	다음 함수를 직접 작성해보세요:

🛠 제공할 기본 함수들
•	addTodo :: String -> State AppState ()
•	markDone :: Int -> State AppState ()
•	removeTodo :: Int -> State AppState ()
•	listTodos :: State AppState [Todo]

-- 새로운 할 일을 추가
addTodo :: String -> State AppState ()
addTodo desc = ...

-- 완료 표시
markDone :: Int -> State AppState ()
markDone tidToMark = ...

-- 할 일 제거
removeTodo :: Int -> State AppState ()
removeTodo tidToRemove = ...

-- 현재 할 일 목록 조회
listTodos :: State AppState [Todo]
listTodos = ...

힌트:
	•	todos %= ...
	•	todos . traversed . filtered (\t -> ...) %~ ...
	•	use todos

⸻

📌 이건 진짜 실전에서 바로 쓸 수 있는 구조예요.
Lens로 각 항목을 수정하고, State로 상태를 유지하며, 나중에는 IO나 ReaderT로 확장 가능합니다.
-}

initAppState :: AppState
initAppState = AppState [] 0

compareId :: Int -> Todo -> Bool
compareId targetId t = t ^. tid == targetId

isDone :: Todo -> Bool
isDone = (^. done)

addTodo :: String -> StateT AppState Identity ()
addTodo desc = do
  nid <- use nextId
  todos %= (\t -> Todo nid desc False : t)
  nextId += 1

markDone :: Int -> StateT AppState Identity ()
markDone targetId = todos . traversed . filtered (compareId targetId) . done .= True

removeTodo :: Int -> StateT AppState Identity ()
removeTodo targetId = todos %= filter (not . compareId targetId)

listTodos :: StateT AppState Identity [Todo]
listTodos = gets $ view todos

todo :: State AppState [Todo]
todo = do
  addTodo "first"
  addTodo "second"
  addTodo "third"
  markDone 0
  removeTodo 1
  listTodos
