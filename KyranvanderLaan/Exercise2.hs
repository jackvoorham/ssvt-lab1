-- Base code imported from Lab 1 at https://canvas.uva.nl/courses/32483/files/7147680?wrap=1&fd_cookie_set=1

module Exercise2 where
import Data.List
import Test.QuickCheck    

prime :: Integer -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
  where xs = takeWhile (\ y -> y^2 <= n) primes

primes :: [Integer]
primes = 2 : filter prime [3..] 

infix 1 --> 
(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

reversal :: Integer -> Integer
reversal = read . reverse . show

data Boy = Matthew | Peter | Jack | Arnold | Carl 
           deriving (Eq,Show)
boys = [Matthew, Peter, Jack, Arnold, Carl]

-- My thoughts:
-- For some reason the length needs to be outputted in Int not Integer (this realisation took some time).
-- I need to learn the notation better since subLength2 occupied a lot of time because I put a wrong declaration.
-- Initial quickcheck gives very slow execution, this is because the powerset become too large to calculate above a certain threshold.
-- However, this is not necessary for the properties.
-- The property we need to test is whether the statement holds for natural numbers.
-- I will do this by using the subsequences function and calculating the length of the original list and the length of the powerset.
-- Q: Is the property hard to test? If you find that it is, can you given a reason why?
-- A: Yes I find this property hard to test because above a length of 25 the powerset and it's length become very large.
--    This means that calculating 2 to the power of this is very slow. This also means that you cannot test all natural numbers using
--    quickcheck.
-- Q: When you perform the test for exercise 4, what are you testing actually? Are you checking a mathematical fact? 
--    Or are you testing whether subsequences satisfies a part of its specification? Or are you testing something else still?
-- A: I believe we are actually testing a subset of natural numbers, namely the natural number 1 until 25. This means we aren't 
--    checking a mathematical fact because we cannot draw a conclusion about all natural numbers from only a small subset.
--    I think that the only thing we can say for certain is that the property holds for all natural number between 1 and 25.
-- This assignment took roughly 1 hour for me.

subLength1 :: [Integer] -> Int
subLength1 n = length (n)

subLength2 :: [Integer] -> Int
subLength2 n = length (subsequences (n))

testsubLength :: [Integer] -> Property
testsubLength n = (length (n) >= 0 && length (n) < 25) ==> subLength2 n == 2^(subLength1 n)

main :: IO ()
main = do
  putStrLn "== Check test =="
  quickCheck testsubLength