-- 4️⃣ Haskell으로 REST API 서버 만들기 (servant 라이브러리 활용)
-- 	•	Haskell을 사용하여 간단한 JSON API 서버를 만들기
-- 	•	예: GET /hello?name=John → { "message": "Hello, John!" }
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Main (main) where

import Data.Aeson (ToJSON)
import GHC.Generics (Generic)
import Lib (mergeSort)
import Network.Wai.Handler.Warp (run)
import Servant
  ( Application,
    Get,
    JSON,
    Proxy (..),
    QueryParam,
    Server,
    serve,
    type (:>),
  )

data Message = Message
  {message :: String}
  deriving (Generic, Show)

instance ToJSON Message

-- 1. API 타입 정의
type API = "hello" :> QueryParam "name" String :> Get '[JSON] Message

-- 2. 서버 구현
server :: Server API
server (Just name) = return $ Message $ "Hello, " ++ name ++ "!"
server Nothing = return $ Message "Hello, Anonymous!"

-- 3. 애플리케이션
api :: Proxy API
api = Proxy

app :: Application
app = serve api server

-- 4. 메인 함수 (서버 실행)
main :: IO ()
main = do
  print $ mergeSort [2, 1, 3]
  putStrLn "Starting server on port 8080..."
  run 8080 app