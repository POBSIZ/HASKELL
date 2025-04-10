module List_Monad_4 (m4) where

-- 4. List Monad – 여러 가능성의 세계
comb :: (Monad m, Num b) => m b -> m b -> m b
comb xs ys = xs >>= \x -> ys >>= \y -> return (x + y)

pairs :: [(Int, Int)]
pairs = do
  x <- [1, 2]
  y <- [10, 20]
  return (x, y)

pairsEvenSum :: [(Int, Int)]
pairsEvenSum = do
  x <- [1, 2]
  y <- [10, 20]
  let s = x + y
  if even s then return (x, y) else []

m4 :: IO ()
m4 = do
  putStrLn $ "-- List Monad"
  print $ comb [1 :: Int, 2] [10, 20] -- [11,21,12,22]
  print $ pairs -- [(1,10),(1,20),(2,10),(2,20)]
  print $ pairsEvenSum -- [(2,10),(2,20)]
  putStrLn $ ""

-- ! Quiz

-- 1️⃣ do { x <- [1,2]; y <- [3,4]; return (x + y) } 결과는?
-- A : [4, 5, 5, 6]

-- 2️⃣ do { x <- [1,2]; y <- []; return (x + y) } 결과는?
-- A : [1, 2] -> X
-- A : [] -> O

-- 3️⃣ do { x <- [1..3]; y <- [1..3]; guard (x < y); return (x, y) } 결과는?
-- A : [(1, 2), (1, 3), (2, 3),]