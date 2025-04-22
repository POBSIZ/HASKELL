{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Todo
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
where

import Control.Lens
  ( filtered,
    makeLenses,
    traversed,
    use,
    view,
    (%=),
    (+=),
    (.=),
    (^.),
  )
import Control.Monad.State (MonadIO (liftIO), StateT, gets)
import Data.Aeson (FromJSON, ToJSON, decode, encode)
import qualified Data.ByteString.Lazy as B
import Data.Time (Day, defaultTimeLocale, parseTimeM)
import GHC.Generics (Generic)
import System.Directory (doesFileExist)

-- 단일 ToDo 항목
data Todo = Todo
  { _tid :: Int,
    _content :: String,
    _done :: Bool,
    _tags :: [String],
    _due :: Maybe Day
  }
  deriving (Show, Generic)

makeLenses ''Todo

instance ToJSON Todo

instance FromJSON Todo

-- 전체 앱 상태
data AppState = AppState
  { _todos :: [Todo],
    _nextId :: Int
  }
  deriving (Show, Generic)

makeLenses ''AppState

instance ToJSON AppState

instance FromJSON AppState

data AppWithHistory = AppWithHistory
  { _current :: AppState,
    _undoStack :: [AppState],
    _redoStack :: [AppState]
  }

makeLenses ''AppWithHistory

type App = StateT AppWithHistory IO

pushUndo :: App ()
pushUndo = do
  prev <- use current
  undoStack %= (prev :)
  redoStack .= []

undo :: App ()
undo = do
  stack <- use undoStack
  if ((length stack) > 20)
    then undoStack %= init
    else case stack of
      (prev : rest) -> do
        curr <- use current
        redoStack %= (curr :)
        current .= prev
        undoStack .= rest
        liftIO $ putStrLn "Undid last action."
      [] -> liftIO $ putStrLn "Nothing to undo."

redo :: App ()
redo = do
  stack <- use redoStack
  if ((length stack) > 20)
    then undoStack %= init
    else case stack of
      (next : rest) -> do
        pushUndo
        current .= next
        redoStack .= rest
        liftIO $ putStrLn "Redid action."
      [] -> liftIO $ putStrLn "Nothing to redo."

filePath :: FilePath
filePath = "todos.json"

saveState :: AppState -> IO ()
saveState _state = B.writeFile filePath (encode _state)

loadState :: IO AppState
loadState = do
  exists <- doesFileExist filePath
  if exists
    then do
      _content <- B.readFile filePath
      case decode _content of
        Just _state -> return _state
        Nothing -> do
          putStrLn "Failed to parse file. Starting fresh."
          return (AppState [] 0)
    else return (AppState [] 0)

compareId :: Int -> Todo -> Bool
compareId targetId t = t ^. tid == targetId

parseDate :: String -> IO (Maybe Day)
parseDate "" = return Nothing
parseDate str = case parseTimeM True defaultTimeLocale "%Y-%m-%d" str of
  Just d -> return (Just d)
  Nothing -> putStrLn "Invalid date!" >> return Nothing

addTodo :: String -> [String] -> Maybe Day -> App ()
addTodo _desc _tags _due = do
  nid <- use (current . nextId)
  pushUndo
  current . todos %= (\t -> Todo nid _desc False _tags _due : t)
  current . nextId += 1

markDone :: Int -> App ()
markDone targetId = pushUndo >> current . todos . traversed . filtered (compareId targetId) . done .= True

removeTodo :: Int -> App ()
removeTodo targetId = pushUndo >> current . todos %= filter (not . compareId targetId)

listTodos :: App [Todo]
listTodos = gets $ view (current . todos)
