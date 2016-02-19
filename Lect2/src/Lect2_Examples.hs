module Lect2_Examples where

-- Функция add - некъри образец
add :: (Int, Int) -> Int
add (x, y) = x + y

-- Функция mult - некъри образец
mult :: (Int, Int, Int) -> Int
mult (x, y, z) = x * y * z

-- Максималното от 3 числа - некъри образец
max3 :: (Int, Int, Int) -> Int
max3 (x, y, z)
    | x >= y && x >= z  = x     -- x е максималното число
    | y >= z            = y     -- y е максималното число
    | otherwise         = z     -- z е максималното число

-- Функция add2 - къри образец
add2 :: Int -> Int -> Int
add2 x y = x + y

-- Функция mult3 - къри образец
mult3 :: Int -> Int -> Int -> Int
mult3 x y z = x * y * z

f :: Float -> Float -> Float
f x y = 
        a^3 + b^2*a - b^3*x*y
   where
         a = 1 + x^4*y^3
         b = x^2 + y^3
         
-- Връща списък от реалните корени на квадратно уравнение
solve :: Double -> Double -> Double -> [Double]
solve a b c
   | d < 0 = []
   | otherwise = [(-b + sqrt d)/2/a, (-b - sqrt d)/2/a]
   where
 d = b*b - 4*a*c

f2 :: Float -> Float -> Float
f2 x y
   | cube_x > cube_y = cube_x
   | otherwise       = cube_y
   where
       cube_x = cube x
       cube_y = cube y
       cube :: Float -> Float
       cube z = z*z*z

{-
solve :: Float -> Float -> Float -> [Float]
solve a b c =
   let
     d = b*b - 4*a*c 
   in 
      if d < 0 then [] else [(-b + sqrt d)/2/a, 
                             (-b - sqrt d)/2/a]
-}

sum2 :: Int -> Int -> Int
sum2 x y =
     sq x + sq y

sq :: Int -> Int
sq z = z*z

{-
-- факториел
fact :: Int -> Int
fact n
  | n == 1  = 1
  | n > 1   = n*fact(n-1)
-}

{-
fact :: Int -> Int
fact n = if n == 1 then 1 else n*fact(n-1)
-}

fact :: Int -> Int
fact n = iter n 1 1
iter :: Int -> Int -> Int -> Int
iter n i p = if i > n then p else iter n (i+1) (p*i)

-- Числа на Фибоначи
fib :: Int -> Int
fib n
  | n == 0  = 0
  | n == 1  = 1
  | n > 1   = fib(n-1) + fib(n-2)





