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

---- Excersise 4 for assignment

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
    