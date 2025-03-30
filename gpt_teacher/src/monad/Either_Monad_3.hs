module Either_Monad_3 (m3) where

-- 3. Either Monad – 실패에 이유를 담다

-- 3-1. Definition
{-
  data Either e a = Left e | Right a
  •	Right a: 성공한 결과
  •	Left e: 실패했을 때의 에러 정보 (e는 설명, 코드 등)
-}

-- 3-2. Diffrence between Maybe and Either
{-
  * When Succed
  - Maybe : Just x
  - Either : Right x

  * When Fail
  - Maybe : Nothing
  - Either : Left "Descriptions"

  * is include failure information
  - Maybe : No
  - Either : Yes
-}

-- ! e.g., (safe div)
safeDivE :: Int -> Int -> Either String Int
safeDivE _ 0 = Left "Cannot divide by zero"
safeDivE a b = Right (a `div` b)

-- ! e.g., (연산 연결 (>>=))
computationE :: Int -> Int -> Either String Int
computationE x y =
  safeDivE x y >>= \r ->
    safeDivE r 2

-- ! e.g., (연산 연결 (do))
computationEDo :: Int -> Int -> Either String Int
computationEDo x y = do
  r1 <- safeDivE x y
  r2 <- safeDivE r1 2
  return r2

-- ! e.g., (실전 예제)
validateName :: String -> Either String String
validateName "" = Left "Name cannot be empty"
validateName name = Right name

validateAge :: Int -> Either String Int
validateAge age
  | age < 0 = Left "Age cannot be negative"
  | otherwise = Right age

createUser :: String -> Int -> Either String (String, Int)
createUser name age = do
  validName <- validateName name
  validAge <- validateAge age
  return (validName, validAge)

m3 :: IO ()
m3 = do
  putStrLn $ "-- Either Monad"

  putStrLn $ "Either 2/2, 2/0"
  print $ safeDivE 2 2
  print $ safeDivE 2 0

  putStrLn $ ""
  putStrLn $ "Either Bind 2/2, 2/0"
  print $ computationE 2 2
  print $ computationE 2 0

  putStrLn $ ""
  putStrLn $ "Either Do 2/2, 2/0"
  print $ computationEDo 2 2
  print $ computationEDo 2 0

  putStrLn $ ""
  putStrLn $ "Create User"
  print $ createUser "Alice" 30
  print $ createUser "" 30
  print $ createUser "Bob" (-5)

  putStrLn $ ""

-- ! Quiz

-- 1️⃣ safeDivE 10 0의 결과는?
-- A : Left "Cannot divide by zero"

-- 2️⃣ safeDivE 10 2 >>= \x -> safeDivE x 0 의 결과는?
-- A : Left "Cannot divide by zero"

-- 3️⃣ do { a <- Right 10; b <- Right 5; return (a * b) } 결과는?
-- A : Right 50

-- 4️⃣ do { a <- Left "fail"; b <- Right 5; return (a * b) } 결과는?
-- A : Left "fail"