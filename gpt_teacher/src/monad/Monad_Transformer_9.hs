module Monad_Transformer_9 (m9) where

import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (ReaderT (runReaderT))
import Control.Monad.Trans.Maybe (MaybeT (MaybeT), runMaybeT)
import Control.Monad.Trans.Reader (asks)

{-
  ✅ Monad Transformer란?

  “여러 개의 모나드를 한꺼번에 써야 할 때 어떻게 하지?”

  ✔️ 예: IO 안에서 Maybe, Either, State, Reader, Writer 등을 동시에 사용하고 싶을 때
  ✔️ 이럴 때 Monad Transformer가 필요합니다

  📦 개념 요약

  각 모나드에는 Transformer 버전이 있습니다:
  Monad   Transformer   Version   Type
  ---
  Maybe   MaybeT        MaybeT    m a
  Either  ExceptT       ExceptT   e m a
  State	  StateT        StateT    s m a
  Reader	ReaderT       ReaderT   r m a
  Writer	WriterT       WriterT   w m a

  ✔️ m은 바깥쪽 모나드 (보통 IO 등)
-}

{-
  ✅ 예시 1: MaybeT IO a
  → 실패할 수 있는 IO 연산
  ✔️ liftIO로 IO 모나드를 들어올림
  ✔️ MaybeT 안에서 return Nothing으로 실패 표현
-}
maybeGetLine :: MaybeT IO String
maybeGetLine = do
  line <- liftIO getLine
  if null line then MaybeT (return Nothing) else return line

{-
  ✅ 예시 2: ReaderT Config (ExceptT String IO) a
  → 환경도 읽고, 에러도 반환하고, IO도 사용
-}
type AppM = ReaderT Config (ExceptT String IO)

data Config = Config {appName :: String, port :: Int}

runApp :: AppM a -> Config -> IO (Either String a)
runApp action cfg = runExceptT (runReaderT action cfg)

appAction :: ReaderT Config (ExceptT String IO) String
appAction = do
  name <- asks appName
  portNum <- asks port
  return $ "Hello from " ++ show name ++ " " ++ show portNum

{-
  ✨ 실습 과제
  이제 하나 직접 만들어보세요!

  ✅ 목표:
  MaybeT IO를 사용해서 사용자 이름을 입력받고,
  빈 문자열이면 실패 (Nothing), 아니면 인사 메시지를 출력.

  ✔️ liftIO를 사용해야 getLine, putStrLn이 작동합니다
  ✔️ MaybeT로 감싸진 함수는 실패 가능성을 포함한 IO 작업입니다
-}

askName :: MaybeT IO String
askName = MaybeT $ do
  line <- getLine
  return $ if null line then Nothing else Just line

greetUser :: MaybeT IO ()
greetUser = do
  liftIO $ putStrLn "Input your name and enter"
  name <- askName
  liftIO $ putStrLn $ "Hi! " ++ name ++ " Greeting!"

m9 :: IO ()
m9 = do
  -- MaybeT & IO
  putStrLn "Input anything and enter"
  result <- runMaybeT maybeGetLine
  case result of
    Just line -> putStrLn $ "You entered: " ++ line
    Nothing -> putStrLn "Empty input detected"

  -- ReaderT & ExceptT & Either
  let config = Config "MyApp" 8080
  result2 <- runApp appAction config
  case result2 of
    Left err -> putStrLn $ "Error: " ++ err
    Right value -> putStrLn value

  -- MaybeT & IO
  result3 <- runMaybeT greetUser
  case result3 of
    Nothing -> putStrLn "No input given."
    Just () -> return ()

{-
  🧠 요약 – Monad Transformer 핵심
  Keyword       Description
  ---
  MaybeT IO     실패할 수 있는 IO 연산
  liftIO  IO    연산을 Transformer 스택 안으로 끌어올림
  runMaybeT     Transformer 실행하기 (결과는 IO (Maybe a))
  MonadTrans    모나드 리프트를 위한 클래스 (lift)
-}