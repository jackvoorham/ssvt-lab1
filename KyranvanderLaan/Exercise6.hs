-- Base code imported from Lab 1 at https://canvas.uva.nl/courses/32483/files/7147680?wrap=1&fd_cookie_set=1

module Exercise6 where
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
-- First thing to take notice of is that the list has to start with 2 meaning we don't have to test prime numbers
-- individually, only the consecutive list of primes.
-- The first time we approached this with a secundairy function which read as follows:
-- firstNPrimes :: Int -> [Integer]
-- firstNPrimes n = if prime(product(take n primes) + 1) /= True
--                   then take n primes
--                   else firstNPrimes (n+1)
-- However, we later came to the conclusion that this only outputs the first example not a list of examples.
-- The list can be made within one function which is how we got to the answer.
-- Time spend: 1 hour

counterexamples ::  [([Integer], Integer)]
counterexamples = [(k, (product (k)) + 1) | k <- [take n primes | n <- [1..], prime(product (take n primes) + 1) /= True]]
