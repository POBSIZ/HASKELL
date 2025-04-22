module Todo_CLI (todoCliApp) where

import Control.Lens (use, (^.))
import Control.Monad (unless)
import Control.Monad.State (MonadIO (liftIO), execStateT)
import Data.List (intercalate)
import System.IO (hFlush, stdout)
import Todo
  ( App,
    AppWithHistory (AppWithHistory),
    Todo,
    addTodo,
    content,
    current,
    done,
    due,
    listTodos,
    loadState,
    markDone,
    parseDate,
    redo,
    removeTodo,
    saveState,
    tags,
    tid,
    undo,
  )

mainApp :: App ()
mainApp = do
  curr <- use current
  liftIO $ saveState curr
  liftIO $ putStrLn "\n--- TODO MENU ---"
  liftIO $ putStrLn "1. Add Todo"
  liftIO $ putStrLn "2. Mark Done"
  liftIO $ putStrLn "3. Remove Todo"
  liftIO $ putStrLn "4. List Todos"
  liftIO $ putStrLn "5. Undo"
  liftIO $ putStrLn "6. Redo"
  liftIO $ putStrLn "7. Exit"
  liftIO $ putStr "Choose: "
  liftIO $ hFlush stdout
  choice <- liftIO getLine
  case choice of
    "1" -> uiAdd
    "2" -> uiMark
    "3" -> uiRemove
    "4" -> uiList
    "5" -> undo
    "6" -> redo
    "7" -> liftIO $ putStrLn "Goodbye!"
    _ -> liftIO $ putStrLn "Input available values!"
  unless (choice == "7") mainApp

uiAdd :: App ()
uiAdd = do
  liftIO $ putStr "New todo: "
  liftIO $ hFlush stdout
  desc <- liftIO getLine

  liftIO $ putStr "Tags (comma-separated): "
  liftIO $ hFlush stdout
  tagsStr <- liftIO getLine
  let _tags = words $ map (\c -> if c == ',' then ' ' else c) tagsStr

  liftIO $ putStr "Due date (YYYY-MM-DD or blank): "
  liftIO $ hFlush stdout
  dueStr <- liftIO getLine
  _due <- liftIO $ parseDate dueStr

  addTodo desc _tags _due
  liftIO $ putStrLn "Added!"

uiMark :: App ()
uiMark = do
  liftIO $ putStr "Todo ID to mark as done: "
  liftIO $ hFlush stdout
  uTid <- read <$> liftIO getLine
  markDone uTid
  liftIO $ putStrLn "Marked done."

uiRemove :: App ()
uiRemove = do
  liftIO $ putStr "Todo ID to remove: "
  liftIO $ hFlush stdout
  uTid <- read <$> liftIO getLine
  removeTodo uTid
  liftIO $ putStrLn "Removed."

uiList :: App ()
uiList = do
  ts <- listTodos
  liftIO $ putStrLn "\n--- TODOS ---"
  mapM_ (liftIO . printTodo) ts

printTodo :: Todo -> IO ()
printTodo t = do
  putStrLn $ show (t ^. tid) ++ ": " ++ if (t ^. done) then "[✔] " else "[ ] "
  putStrLn $ t ^. content
  unless (null (t ^. tags)) $ putStrLn $ "Tags: " ++ intercalate ", " (t ^. tags)
  case t ^. due of
    Just d -> putStrLn $ "Due: " ++ show d ++ "\n"
    Nothing -> putStrLn ""

todoCliApp :: IO ()
todoCliApp = do
  initialState <- loadState
  finalState <- execStateT mainApp (AppWithHistory initialState [] [])
  saveState (finalState ^. current)
