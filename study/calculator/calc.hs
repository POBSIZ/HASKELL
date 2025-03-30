main :: IO ()
main = do
  putStrLn "Simple Calculator"
  putStrLn "Enter an expression (e.g., 3 + 4):"
  input <- getLine
  let result = eval input
  case result of
    Left err -> putStrLn $ "Error: " ++ err
    Right answer -> putStrLn $ "Result: " ++ show answer

-- Evaluate the input expression
eval :: String -> Either String Double
eval input =
  case words input of
    [x, "+", y] -> safeRead x +. safeRead y
    [x, "-", y] -> safeRead x -. safeRead y
    [x, "*", y] -> safeRead x *. safeRead y
    [x, "/", y] -> safeRead x /. safeRead y
    _ -> Left "Invalid input format. Use: [number] [operator] [number]"

-- Helper functions for safe arithmetic
safeRead :: String -> Either String Double
safeRead s = case reads s :: [(Double, String)] of
  [(n, "")] -> Right n
  _ -> Left $ "Cannot parse number: " ++ s

(+.) :: Either String Double -> Either String Double -> Either String Double
(+.) (Right a) (Right b) = Right (a + b)
(+.) (Left err) _ = Left err
(+.) _ (Left err) = Left err

(-.) :: Either String Double -> Either String Double -> Either String Double
(-.) (Right a) (Right b) = Right (a - b)
(-.) (Left err) _ = Left err
(-.) _ (Left err) = Left err

(*.) :: Either String Double -> Either String Double -> Either String Double
(*.) (Right a) (Right b) = Right (a * b)
(*.) (Left err) _ = Left err
(*.) _ (Left err) = Left err

(/.) :: Either String Double -> Either String Double -> Either String Double
(/.) (Right a) (Right 0) = Left "Division by zero"
(/.) (Right a) (Right b) = Right (a / b)
(/.) (Left err) _ = Left err
(/.) _ (Left err) = Left err
