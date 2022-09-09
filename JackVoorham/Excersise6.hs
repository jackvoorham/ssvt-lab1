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

-- Create prime subsets starting from 2 with n the start length
createPrimeSubsets :: Int -> [[Integer]]
createPrimeSubsets n = take n primes : createPrimeSubsets (succ n)

---- Excersise 6 for assignment
counterexamples ::  [([Integer], Integer)] 
counterexamples = [(xs, product xs + 1) | xs <- createPrimeSubsets 1, not $ prime (product xs + 1)] 

---- Reflection
-- This excersise went quite easy as i combined multiple elements from the previous excersises, that is,
-- the createPrimeSubsets is inspired by excersise5's function with the same name. And the counterexamples
-- list generation is using the same syntax as in 4 with the reversiblestream function.

---- Time taken: 45 minutes
    