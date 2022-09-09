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

---- Excersise 2 for assignment

listPowersetEquality :: [Int] -> Property 
listPowersetEquality xs = (len >= 0 && len < 30) ==> 2^len == pLen where 
  len = length xs -- Length of list
  pLen = length (subsequences xs) -- Number of subsets 

main :: IO ()
main = do
    putStrLn "== Testing Excersise 2 =="
    quickCheck listPowersetEquality
    
---- Reflection

-- Q: Is the property hard to test? If you find that it is, can you given a reason why?
-- A: Yes it hard to test, because computing the powerset grows with O(n*2^n) complexity
-- meaning that after a certain length, powerset generation is going to take extremely long
-- thus, testing the property for many lists takes a lot of time. I currently get to 44 tests,
-- then the tests make none to slow progress when running the quickTest without preconditions.
-- When adding a precondition that the length of the list must be between 0 and 30 the the 
-- test passes. We thus only test for a limited list size.

-- Q: Give your thoughts on the following issue: when you perform the test for exercise 4, 
-- what are you testing actually? Are you checking a mathematical fact? Or are you testing 
-- whether subsequences satisfies a part of its specification? Or are you testing something 
-- else still?
-- A: We are not testing a mathematical fact. We are testing whether subsequences satifies a 
-- part of its specification. Specifically, we are testing whether the property holds for lists
-- with a length n >= 0 and <= 30. We are not testing for a mathematical fact because we only limit
-- ourselves to n being a small part of the natural numbers, and we are not proving for all n.

----  Time: Took around 30 minutes to complete