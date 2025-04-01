module Monad_Transformer_Stack_10 (m10) where

import Control.Monad.Except (ExceptT, MonadError (catchError, throwError), runExceptT)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (ReaderT (runReaderT))
import Control.Monad.Trans.Reader (asks)

{-
  🎯 목표: ReaderT + ExceptT + IO 조합

  우리가 만들 프로그램은 다음과 같습니다:

  사용자 설정(Config)에서 값을 읽고,
  입력을 받아 숫자로 파싱하고,
  0으로 나누면 오류를 반환하고,
  최종 결과를 IO로 출력
-}

{-
  ✅ 1. 환경 정의
-}
data Config = Config {appName :: String, defaultDivisor :: Double}

{-
  ✅ 2. 타입 스택 정의
	•	ReaderT → 환경 전달
	•	ExceptT → 에러 핸들링
	•	IO → 입력과 출력
-}
type AppM = ReaderT Config (ExceptT String IO)

{-
  ✅ 3. 프로그램 흐름
	•	사용자에게 숫자 입력 받기 (liftIO)
	•	defaultDivisor로 나눔
	•	결과 출력 or 에러 메시지 반환

  예시 흐름:
  Welcome to AppX!
  Enter a number to divide:
  10
  Result: 5.0
-}

getInputNumber :: AppM Double
getInputNumber = do
  liftIO $ putStrLn "Enter a number to divide:"
  input <- liftIO $ getLine
  if all (`elem` "0123456789") input
    then return $ read input
    else throwError "Invalid number!"

divideAndReport :: Double -> AppM (Double)
divideAndReport a = do
  divisor <- asks defaultDivisor
  if divisor > 0
    then return $ a / divisor
    else throwError "Division by zero!"

mainApp :: AppM ()
mainApp = do
  name <- asks appName
  liftIO $ putStrLn $ "Welcome to " ++ name ++ "!"
  input <- getInputNumber `catchError` handleError
  result <- divideAndReport input `catchError` handleError
  liftIO $ putStrLn $ "Result: " ++ show result
  where
    handleError :: String -> AppM a
    handleError err = do
      liftIO $ putStrLn err
      throwError err

m10 :: IO (Either String ())
m10 = do
  runExceptT $ runReaderT mainApp (Config "AppX" 2)
