-- Base code imported from Lab 1 at https://canvas.uva.nl/courses/32483/files/7147680?wrap=1&fd_cookie_set=1

module Lab1 where
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
-- This threshold is around the powerset of the list of 1 through 29.


subLength1 :: [Integer] -> Int
subLength1 n = length (n)

subLength2 :: [Integer] -> Int
subLength2 n = length (subsequences (n))

testsubLength :: [Integer] -> Bool
testsubLength n = subLength2 n == 2^(subLength1 n)