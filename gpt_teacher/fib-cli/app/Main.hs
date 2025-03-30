module Main (main) where

import Data.List (intercalate)
import Lib (parMapFib)
import System.Environment (getArgs)
import Text.Read (readMaybe)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> putStrLn "사용법: fib-cli [..args]"
    _ -> case mapM readMaybe args :: Maybe [Integer] of
      Nothing -> putStrLn "모든 인자는 유효한 숫자여야 합니다."
      Just ns -> putStrLn $ intercalate ", " $ map show $ parMapFib ns