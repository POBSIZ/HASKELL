module Reader_Monad_7 (m7) where

{-
  ✅ Reader Monad – 읽기 전용 환경을 전달하라

  “매번 인자를 넘기지 않고, 함수 내부에서 ‘공용 환경’에 접근할 수 없을까?”

  ✔️ Reader Monad는 그런 환경을 다룰 때 사용됩니다.
  예를 들어:
    •	설정값 (config)
    •	공용 변수 (context)
    •	요청 정보 (Request)

  📦 개념
  newtype Reader r a = Reader {runReader :: r -> a}
	•	r: 환경 (읽기 전용 값)
	•	a: 결과
	•	Reader r a는 “r이라는 환경을 받아서 a를 계산하는 함수”라고 보면 돼요.

  예시
  Reader String Int  -- 문자열 환경을 읽어서 정수 결과를 내는 계산

  ✅ 기본 함수

  ask – 현재 환경을 가져옴
  ask :: Reader r r

  local – 환경을 일시적으로 바꿔서 하위 연산 실행
  local :: (r -> r) -> Reader r a -> Reader r a
-}

-- 🛠 예제: 사용자 설정에서 이름 가져오기
import Control.Monad.Reader

type GConfig = String

greet :: Reader GConfig String
greet = do
  name <- ask
  return ("Hello, " ++ name ++ "!")

-- ✅ 예제: 복합 환경 사용
data Env = Env {user :: String, isAdmin :: Bool}

greeting :: Reader Env String
greeting = do
  u <- asks user
  admin <- asks isAdmin
  return $
    if admin
      then "Welcome back, admin " ++ u ++ "!"
      else "Hello, " ++ u

-- ✨ 실습 과제
-- 아래 코드를 직접 채워보세요!
data Config = Config {appName :: String, port :: Int}

-- 포트 번호를 출력하는 함수
showPort :: Reader Config String
showPort = do
  _port <- asks port
  return $ show _port

-- 앱 이름과 포트를 조합하여 문장을 만드는 함수
-- runReader appInfo (Config "MyApp" 8080)
-- 결과: "App 'MyApp' is running on port 8080."
appInfo :: Reader Config String
appInfo = do
  name <- asks appName
  portStr <- showPort
  return $ "App '" ++ name ++ "' is running on port " ++ portStr ++ "."

{-
  🔁 핵심 요약 – Reader Monad
  - Reader r a      : 환경 r에서 계산하여 결과 a를 반환하는 구조
  - ask             : 전체 환경 값을 가져옴
  - asks f          : 환경에서 필요한 부분만 추출
  - local f action  : 일시적으로 환경을 바꿔서 실행
-}

-- 🧠 도전 과제 (선택)
-- runReader withCustomPort (Config "AppX" 8080)
-- 결과: "App 'AppX' is running on port 9999."
withCustomPort :: Reader Config String
withCustomPort = local (\cfg -> cfg {port = 9999}) appInfo

withCustomAppName :: Reader Config String
withCustomAppName = local (\cfg -> cfg {appName = "app"}) appInfo

m7 :: IO ()
m7 = do
  -- 결과: "Hello, Alice!"
  print $ runReader greet "Alice"

  -- "Welcome back, admin Alice!"
  print $ runReader greeting (Env "Alice" True)

  -- 결과: "App 'MyApp' is running on port 8080."
  print $ runReader appInfo (Config "MyApp" 8080)

  -- 결과: "App 'AppX' is running on port 9999."
  print $ runReader withCustomPort (Config "AppX" 8080)

  -- 결과: "App 'app' is running on port 8080."
  print $ runReader withCustomAppName (Config "AppX" 8080)