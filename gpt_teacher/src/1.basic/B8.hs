module B8 (b8Print) where

import Data.Map as Map
import Data.Set as Set

-- 1️⃣ groupByTwo: 리스트를 두 개씩 묶어서 튜플 리스트로 반환 (zip 활용)
-- 	•	예: groupByTwo [1,2,3,4,5,6] → [(1,2), (3,4), (5,6)]
-- 	•	홀수 개의 요소가 있으면 마지막 요소는 무시됨.
groupByTwo :: [a] -> [(a, a)]
groupByTwo xs = go xs id -- Difference List 적용
  where
    go (x : y : rest) acc = go rest (acc . ((x, y) :))
    go _ acc = acc []

-- 2️⃣ removeDuplicates: 리스트에서 중복 요소 제거 (foldl 활용)
-- 	•	예: removeDuplicates [1,2,2,3,3,3,4,5] → [1,2,3,4,5]
removeDuplicates :: (Ord a) => [a] -> [a]
removeDuplicates xs = go xs Set.empty id
  where
    go [] _ acc = acc []
    go (y : ys) seen acc
      | y `Set.member` seen = go ys seen acc
      | otherwise = go ys (Set.insert y seen) (acc . (y :))

-- 3️⃣ runningSum: 리스트에서 현재까지의 합을 담은 리스트 반환 (scanl 활용)
-- 	•	예: runningSum [1,2,3,4] → [1,3,6,10]
runningSum :: [Int] -> [Int]
runningSum = Prelude.drop 1 . scanl (+) 0

-- 4️⃣ frequencyMap: 리스트의 각 요소가 몇 번 등장했는지 카운트 (foldl 활용)
-- 	•	예: frequencyMap "hello" → [('h',1), ('e',1), ('l',2), ('o',1)]
-- 	•	힌트: Data.Map을 사용할 수도 있음!
frequencyMap :: (Ord a) => [a] -> [(a, Int)]
frequencyMap xs = Map.toList (Prelude.foldl updateCount Map.empty xs)
  where
    updateCount m c = Map.insertWith (+) c 1 m

b8Print :: IO ()
b8Print = do
  print $ groupByTwo [(1 :: Integer), 2, 3, 4, 5, 6]
  print $ removeDuplicates [(1 :: Integer), 2, 2, 3, 3, 3, 4, 5]
  print $ runningSum [1, 2, 3, 4]
  print $ frequencyMap "hello"
