module Examples2 where

-- function max2
max2 :: Int -> Int -> Int
max2 x y = if x >= y then x else y

-- function max3
max3 :: Int -> Int -> Int -> Int
max3 x y z =
           if x >= y && x >= z then x
           else if y >= z then y
                else z

