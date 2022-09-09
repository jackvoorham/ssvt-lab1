-- Base code imported from Lab 1 at https://canvas.uva.nl/courses/32483/files/7147680?wrap=1&fd_cookie_set=1

module Exercise5 where
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
-- We will again use the provided prime and primes functions to get the list of primes and check whether
-- the sums are primes.
-- Personally the problem for my lies in the fact that I cannot figure out how to drop one element and then take the 
-- next 101 elements from an infinite list which costs me a lot of time.
-- This is eventually fixed by creating a secundary function which recursively drops the first n elements of primes starting at 0
-- and increasing by 1 with each iteration untill a solution has been found.
-- The answer relies on whether the dropping of the first n elements and taking the next 101 of the primes list is correct.
-- Time spend: 1.5 hours.

return101Primes :: Int -> Integer
return101Primes n = if prime(sum(drop n(take (n+101) (primes)))) == True
                      then sum(drop n(take (n+101) (primes)))
                      else return101Primes (n+1)

consecutive101Prime :: Integer
consecutive101Prime = return101Primes 0
