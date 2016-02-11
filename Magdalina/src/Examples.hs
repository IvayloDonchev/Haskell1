module Examples where

import Data.Char

max :: Int -> Int -> Int
max x y
   | x >= y = x
   | otherwise = y
   
{-
add :: (Int, Int) -> Int
add (x, y) = x + y
-}

add :: Int -> Int -> Int
add x y = x+y

mult :: Int -> Int -> Int ->Int
mult x y z = x*y*z

max3 :: Int -> Int -> Int ->Int
max3 x y z
  | x >= y && x >= z = x
  | y >= z = y
  | otherwise = z

{-
fact :: Int -> Int
fact n
    | n == 0 = 1
    | n > 1  = n*fact(n-1)
-}

{-
fact :: Int -> Int
fact n = iter n 1 1

-}
iter :: Int -> Int -> Int -> Int
iter n i p = if i > n then p else iter n (i+1) (p*i)

{-
maxEl [x] = x
maxEl (h:t) = if h > mt
              then h
              else mt
   where mt = maxEl t
-}

maxEl :: [Int] -> Int
maxEl lst
      | null (tail lst) = head lst
      | otherwise = Examples.max (head lst) (maxEl (tail lst))

isDigit :: Char -> Bool
isDigit ch = ('0' <= ch) && (ch <= '9')

cc :: Char  -- деклариране на променлива
cc = toUpper 'h'

--fMult :: [Int] -> Int
fMult [] = 1
fMult (x:y) = x * fMult y

-- Проверява дали число съдържа цифрата 2
digit2 :: Int -> Bool
digit2 n
       | n < 0 = digit2 (-n)
       | n == 0 = False
       | (n `mod` 10) == 2 = True
       | otherwise = digit2 (n `div` 10)

-- n-тото число на Фибоначи
fib :: Int -> Int
fib n
      | n==0  = 1
      | n==1  = 1
      | n>1   = fib (n-1) + fib (n-2)

fact :: Integer -> Integer
fact n
     | n == 0 = 1
     | n > 0 = fact(n-1) * n
     | otherwise = 0

sort2 :: (Int, Int) -> (Int, Int)
sort2 (x, y) = if x <= y then (x, y) else (y, x)

toEnd :: a -> [a] -> [a]
toEnd x l = l ++ [x]

sumPairs :: [(Int, Int)] -> [Int]
sumPairs pL = [x+y | (x, y) <- pL]

sumMonPairs :: [(Int, Int)] -> [Int]
sumMonPairs pL = [x+y | (x, y) <- pL, x <= y]

digits :: String -> String
digits str = [ch | ch <- str, ch >= '0', ch <= '9']

allEven x = x == [a | a <- x, even a]

qsort [] = []
qsort (x:xs) = qsort less ++ [x] ++ qsort more
     where less = filter (<x) xs
           more = filter (>=x) xs

square :: Int -> Int
square x = x*x


