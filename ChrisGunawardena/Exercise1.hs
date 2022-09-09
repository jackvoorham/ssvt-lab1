-- 1. Redo exercises 2 and 3 of Workshop 1 by writing QuickCheck tests for these statements.




-- The idea is to test the function  by manually adding numbers^2 from 1 to n
-- I googled haskell sum list function https://stackoverflow.com/questions/51279298/haskell-sum-up-the-first-n-elements-of-a-list


import Data.List
import Test.QuickCheck

sumListSq :: [Integer] -> Integer
sumListSq ([]) = 0
sumListSq (x:[]) = x*x
sumListSq (x:xs) = x*x + sumListSq xs

f2 :: Integer -> Integer
f2 n = div (n * (n+1) * ((2*n)+1))  6


test :: Integer -> Bool
test n = let x = abs n in f2 n == sumListSq [1..abs n]


main :: IO ()
main = do
    quickCheck test

-- I'm getting *** Failed! Falsified (after 5 tests and 2 shrinks), I think its somthing to do with negetive numbers


-- I spent about 5 hours on this because this is the first time I'm using haskell and took me awhile to understand some stuff
