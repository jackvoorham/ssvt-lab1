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

---- Comments:
-- Everyone's individual codes: https://github.com/jackvoorham/ssvt-lab1

---- Jack Voorham:
-- Time spent: 1 hours

---- Kyran van der Laan:
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
-- Time spend: 1 hour.

---- Arie Bisfki:
-- gather reusable functions
-- compose final function
-- the word stream and the lack of arguments indicated that the function needed to be an infinite list
-- in my mind I was also already thinking about filter because we accept only some of the numbers between 1 and 10000,
-- namely on the condition that they are prime and that their reversal is also prime
-- Lost some time on testing the reversibleStream
-- Time spent: 20 min

---- Chris Gunawardena:
-- The was to use filter to filter all primes by the function reversal and check if its also a prime.
-- maybe we can test this by getting a list of primes and checking for reversal primes in this list

----------------------------------------------------------------

-- The outpout of reversibleStream is a list that has numbers that are primes
-- with also the reverse of the number being a prime. E.g., 13 and 31.
reversibleStream :: [Integer]
reversibleStream = [x | x <- [1..10000], prime x, prime $ reversal x]

-- First we test the reversal function, we do this be reversing twice, if the
-- function is correct, we must get back the original value
-- Some exceptions happen if the number has leading 0's, the reversal cant get
-- back to original because the zero's will be lost in the process, we neglect
-- this edge case in the test by checking if k modulo 10 and skip it if true

checkReversal :: Integer -> Property
checkReversal k = (k >= 1 && k `mod` 10 /= 0) ==> reversal (reversal k) == k

-- First property: the list with its reversals contains only prime numbers

propReversibleStream :: [Integer] -> Bool
propReversibleStream f = forall reversibleStream' prime where
  reversibleStream' = map reversal f

main :: IO ()
main = do
    putStrLn "== Testing the given reversal function =="
    quickCheck checkReversal
    putStrLn "== Testing Excersise 4 =="
    quickCheck $ propReversibleStream reversibleStream

---- Reflection
-- N/A

---- Time spent: 1 hours
