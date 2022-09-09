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

---- Comments:
---- Jack Voorham:
-- I found this problem to be pretty hard, especially not having too much experience in Haskell.
-- The main problem for me was the problem on how to simulate a for loop like construction so we 
-- we could traverse primes 1-101 and then 2-102 etc. etc. till we find a range that ultimately 
-- sums up to a prime. Ultimately i solved it by creating a new list which contains subsets of 
-- n length and then filtering the subsets by their summation of being prime. Then ofcourse taking
-- the first subset would be smallest as this one contains the lowest range of numbers, which 
-- ofcourse sums up to a smaller number by definition. I am curious however what the perfect solution
-- for this problem would look like as it feels like it could be solved better.
-- Q: Do you have to test that your answer is correct? How could this be checked?
-- A: We can test the createPrimeSubsets function with some properties, for example by checking
-- whether the length of the subsets indeed is n. For consecutive101Prime we mostly used built in
-- Haskell functions so if createPrimeSubsets is correctly implemented consecutive101Prime would
-- give us the correct answer if those are implemented correctly.
-- Time taken: 1.5 hours

---- Kyran van der Laan:
-- We will again use the provided prime and primes functions to get the list of primes and check whether
-- the sums are primes.
-- Personally the problem for my lies in the fact that I cannot figure out how to drop one element and then take the 
-- next 101 elements from an infinite list which costs me a lot of time.
-- This is eventually fixed by creating a secundary function which recursively drops the first n elements of primes starting at 0
-- and increasing by 1 with each iteration untill a solution has been found.
-- The answer relies on whether the dropping of the first n elements and taking the next 101 of the primes list is correct.
-- Time spend: 1.5 hours.

---- Arie Bisfki:
-- No time left

---- Chris Gunawardena:
-- No time left

-------------------------------------------------------------------------------
-- Function for creating subset of primes of n length
createPrimeSubsets :: Int -> Int -> [[Integer]]

-- Drop first n' - n of taken, that is, we remove the first x elements in front of the subset that 
-- we want. We then append this subset to the set of prime subsets of n length.
-- Parameters are n, the needed subset length and n', a duplicate of n which gets used for creating
-- the next subset of length n. Function takes the first n' elements of the primes, then drops the 
-- first n'-n elements, so that a subset of length n gets created. 

createPrimeSubsets n n' = drop (n' - n) (take n' primes) : createPrimeSubsets n (succ n')

-- We take the first (head) of the subsets created with createPrimeSubsets that is filtered
-- based on the sum of the elements in the subset being a prime number.

consecutive101Prime :: Integer
consecutive101Prime = sum $ head $ filter (prime . sum) (createPrimeSubsets 101 101)
