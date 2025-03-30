module B1 (b1Print) where

radius :: Double
radius = 10

area :: Double
area = pi * radius ^ (2 :: Int)

sumTwo :: Int -> Int -> Int
sumTwo a b = a + b

triple :: Int -> Int
triple a = a * 3

b1Print :: IO ()
b1Print = do
  print ("area : " ++ show area)
  print (sumTwo 1 2)
  print (triple 1)