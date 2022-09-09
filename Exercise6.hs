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

---- Comments:
-- Everyone's individual codes: https://github.com/jackvoorham/ssvt-lab1

---- Jack Voorham:
-- This excersise went quite easy as i combined multiple elements from the previous excersises, that is,
-- the createPrimeSubsets is inspired by my excersise5's function with the same name. And the counterexamples
-- list generation is using the same syntax as in 4 with the reversiblestream function.
-- Time taken: 45 minutes

---- Kyran van der Laan:
-- First thing to take notice of is that the list has to start with 2 meaning we don't have to test prime numbers -- individually, only the consecutive list of primes.
-- The first time we approached this with a secundairy function which read as follows:
-- firstNPrimes :: Int -> [Integer]
-- firstNPrimes n = if prime(product(take n primes) + 1) /= True
--                   then take n primes
--                   else firstNPrimes (n+1)
-- However, we later came to the conclusion that this only outputs the first example not a list of examples.
-- The list can be made within one function which is how we got to the answer.
-- Time spend: 1 hour

-- Chris Gunawardena:
-- No time left

-- Arie Bisfki:
-- No time left

--------------------------------------------------------------------

-- Create prime subsets starting from 2 with n the start length
createPrimeSubsets :: Int -> [[Integer]]
createPrimeSubsets n = take n primes : createPrimeSubsets (succ n)

---- Excersise 6 for assignment
counterexamples ::  [([Integer], Integer)]
counterexamples = [(xs, product xs + 1) | xs <- createPrimeSubsets 1, not $ prime (product xs + 1)]

