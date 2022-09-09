-- Base code imported from Lab 1 at https://canvas.uva.nl/courses/32483/files/7147680?wrap=1&fd_cookie_set=1

module Exercise4 where
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
-- First I'll answer the question about how to test whether the reverse function works.
-- We can test this by reversing a number twice and checking whether that equals the first number.
-- If this proves correct, the function works as intended.
-- One thing we noticed is that the reversal of a negative number doesn't work. However, this is not important since we only
-- need to test prime numbers.
-- We use the proved function primes to get a list of all the prime numbers to test and the function prime to test if the
-- reversal of a number is also a prime.
-- For some reason adding k < 10000 to the conditions of the range doesn't actually end the range it just hangs at the last number with k < 10000.
-- For this reason we added the takeWhile statement which takes inspiration from the takeWhile functionality explanation at:
-- https://www.haskelltutorials.com/guides/haskell-lists-ultimate-guide.html

reversibleStream :: [Integer]
reversibleStream = takeWhile (\x -> x < 10000) [k | k <- primes, prime(reversal(k)) == True]
