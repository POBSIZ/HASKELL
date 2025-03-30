module B5 (b5Print) where

-- 1️⃣ incrementAll: 리스트의 모든 요소를 1 증가시키는 함수
-- 	•	예: incrementAll [1,2,3] → [2,3,4]
-- 	•	힌트: map과 람다 함수 \x -> x + 1 사용
incrementAll :: [Int] -> [Int]
incrementAll = map (+ 1)

-- 2️⃣ multiply: 두 숫자를 곱하는 함수 (커링 활용)
-- 	•	예: multiply 3 4 → 12
-- 	•	triple = multiply 3 를 정의하고 triple 5 → 15 가 나오도록 구현
multiply :: Int -> Int -> Int
multiply = (*)

-- 3️⃣ filterShortWords: 길이가 3 이하인 단어만 필터링
-- 	•	예: filterShortWords ["hi", "hello", "ok", "yes", "no"] → ["hi", "ok", "no"]
-- 	•	힌트: filter와 \x -> length x <= 3 사용
filterShortWords :: [String] -> [String]
filterShortWords = filter ((<= 3) . length)

b5Print :: IO ()
b5Print = do
  print (incrementAll [1, 2, 3])
  print (multiply 3 4)
  print (filterShortWords ["hi", "hello", "ok", "yes", "no"])