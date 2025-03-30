module IO_Monad_5 (m5) where

-- 5. IO Monad

{-
  | Haskell의 IO는 다음과 같은 특성을 가집니다:
	-	IO a는 a를 계산할 수 있는 “행위”를 나타내는 값
	-	이 행위 자체가 값으로 존재하며, 나중에 실행됨
	-	Haskell은 IO 블록 안에서만 실제 세계와 상호작용함
-}

readName :: IO String
readName = do
  putStrLn "What is your name?"
  name <- getLine
  return name

askAge :: IO Int
askAge = do
  putStrLn "How old are you?"
  input <- getLine
  return (read input :: Int)

askAgeMonad :: IO Int
askAgeMonad = getLine >>= \input -> return (read input :: Int)

m5 :: IO ()
m5 = do
  putStrLn $ "-- List Monad"
  name <- readName
  age1 <- askAge
  age2 <- askAgeMonad
  putStrLn $ "Hi " ++ name ++ "!"
  putStrLn $ "Your age is " ++ show age1 ++ " right?"
  putStrLn $ "Your age is " ++ show age2 ++ " right?"
  putStrLn $ ""

-- ! Quiz

-- 1️⃣ putStrLn "Hello" 의 타입은?
-- A : IO String -> X
-- A : IO () -> O

-- 2️⃣ getLine >>= \x -> return (length x) 의 결과 타입은?
-- A : IO Int

-- 3️⃣ 아래 코드를 실행하면 출력되는 마지막 문장은?
-- main = do
--   putStrLn "Name?"
--   name <- getLine
--   putStrLn ("Hi " ++ name)
-- A : "Hi <입력한 이름>"

-- 4️⃣ do 블록 없이 IO Monad 코드를 연결하려면 어떤 연산자를 써야 할까요? (>>= or …?)
-- A : >>=

-- ! Quiz

-- 🎯 문제: 아래 do 블록을 >>=로 변환해보세요

-- greet :: IO ()
-- greet = do
--   putStrLn "Your name?"
--   name <- getLine
--   putStrLn ("Hi " ++ name)

-- A :
-- greet :: IO ()
-- greet = putStrLn "Your name?" >> getLine >>= \line -> putStrLn $ "Hi " ++ line

-- echoTwice :: IO ()
-- echoTwice = do
--   putStrLn "Say something:"
--   line <- getLine
--   putStrLn line
--   putStrLn line

-- A :
-- echoTwice :: IO ()
-- echoTwice = putStrLn "Say something:" >> getLine >>= \line -> putStrLn line >> putStrLn line