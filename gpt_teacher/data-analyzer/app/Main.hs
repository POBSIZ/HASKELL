-- 3️⃣ Haskell 기반 데이터 분석 도구 (Data.Map, Text 활용)
-- 	•	파일에서 단어 빈도를 분석하고, 가장 많이 등장한 단어를 찾아 출력
-- 	•	예: wordCount "textfile.txt" → ("the", 1000)

module Main (main) where

import Data.Char (isSpace)
import Data.List.Split (splitOn)
import Data.Map (empty, insertWith, toList)
import Data.Maybe (listToMaybe)
import Text.StringLike (StringLike)

removeEmptyAndBlank :: [String] -> [String]
removeEmptyAndBlank = filter $ not . all isSpace

frequencyMap :: (Ord a) => [a] -> [(a, Int)]
frequencyMap xs = toList $ foldl updateCount empty xs
  where
    updateCount m c = insertWith (+) c 1 m

mergeSort :: (StringLike a) => (Integral b) => [(a, b)] -> [(a, b)]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = merge (mergeSort left) $ mergeSort right
  where
    mid = length xs `div` 2
    (left, right) = splitAt mid xs
    merge mxs [] = mxs
    merge [] mxs = mxs
    merge (x1 : xs1) (x2 : xs2)
      | snd x1 >= snd x2 = x1 : merge xs1 (x2 : xs2)
      | otherwise = x2 : merge (x1 : xs1) xs2

getWords :: String -> [[Char]]
getWords contents = splitOn " " $ unwords $ removeEmptyAndBlank $ lines contents

getWordsFrequencyMap :: String -> [([Char], Int)]
getWordsFrequencyMap contents = frequencyMap $ getWords contents

main :: IO ()
main = do
  contents <- readFile "textfile.txt"
  let sortedWordsMap = mergeSort $ getWordsFrequencyMap contents

  case listToMaybe sortedWordsMap of
    Just mostFrequent -> print mostFrequent
    Nothing -> putStrLn "The file is empty or contains no valid words."