module Todo_CLI (todoCliApp) where

import Control.Lens
import Control.Monad.State
import Todo
  ( AppState,
    Todo,
    addTodo,
    content,
    done,
    initAppState,
    listTodos,
    markDone,
    removeTodo,
    tid,
  )

{-
완벽합니다! 😎
이제 여러분의 상태 시스템에 간단한 터미널 기반 UI (Console UI) 를 붙여봅시다.
우리는 IO와 State를 결합한 StateT AppState IO 모나드에서 Lens를 활용하여 입력/출력/상태 조작을 완성할 거예요.

⸻

🎯 목표: Todo 리스트 CLI 앱 만들기

✅ 가능한 기능들
	•	1 : 할 일 추가
	•	2 : 완료 처리
	•	3 : 삭제
	•	4 : 전체 보기
	•	5 : 종료

✍️ 실습 미션
1.	위 구조를 파일에 붙여서 전체 CLI Todo 앱을 완성해보세요
2.	실제로 실행하고 직접 Todo를 추가/완료/삭제 해보세요
3.	결과를 공유해주시면, 이후 기능 확장 (파일 저장, Undo, Tag)도 도와드릴게요
-}
-- 1. 타입 정의

type App = StateT AppState IO

-- Helper function to lift State AppState to StateT AppState IO
liftState :: State AppState a -> App a
liftState = mapStateT (return . runIdentity)

-- 2. 인터랙션 루프

mainApp :: App ()
mainApp = do
  liftIO $ putStrLn "\n--- TODO MENU ---"
  liftIO $ putStrLn "1. Add Todo"
  liftIO $ putStrLn "2. Mark Done"
  liftIO $ putStrLn "3. Remove Todo"
  liftIO $ putStrLn "4. List Todos"
  liftIO $ putStrLn "5. Exit"
  liftIO $ putStr "Choose: "
  choice <- liftIO getLine
  case choice of
    "1" -> uiAdd
    "2" -> uiMark
    "3" -> uiRemove
    "4" -> uiList
    "5" -> liftIO $ putStrLn "Goodbye!"
    _ -> liftIO $ putStrLn "Goodbye!"
  if not $ choice == "5"
    then mainApp
    else liftIO $ putStr ""

uiAdd :: App ()
uiAdd = do
  liftIO $ putStr "New todo: "
  desc <- liftIO getLine
  liftState $ addTodo desc
  liftIO $ putStrLn "Added!"

uiMark :: App ()
uiMark = do
  liftIO $ putStr "Todo ID to mark as done: "
  uTid <- read <$> liftIO getLine
  liftState $ markDone uTid
  liftIO $ putStrLn "Marked done."

uiRemove :: App ()
uiRemove = do
  liftIO $ putStr "Todo ID to remove: "
  uTid <- read <$> liftIO getLine
  liftState $ removeTodo uTid
  liftIO $ putStrLn "Removed."

uiList :: App ()
uiList = do
  ts <- liftState listTodos
  liftIO $ putStrLn "\n--- TODOS ---"
  mapM_ (liftIO . printTodo) ts

printTodo :: Todo -> IO ()
printTodo t = do
  putStrLn $ show (t ^. tid) ++ ": " ++ t ^. content ++ if t ^. done then " [✔]" else ""

-- 4. 메인 함수
todoCliApp :: IO ()
todoCliApp = evalStateT mainApp initAppState