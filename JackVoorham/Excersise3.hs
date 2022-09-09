module Exercise3 where
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

---- Excersise 3 for assignment

perms :: [a] -> [[a]]
perms [] = [[]]
perms (x:xs) = concat (map (insrt x) (perms xs)) where
  insrt x [] = [[x]]
  insrt x (y:ys) = (x:y:ys) : map (y:) (insrt x ys)

-- We know that the list has n distinct elements so we 
-- we can state that the num. of permutations must be n!

-- Pattern matching approach for computing factorial
-- Based on https://stackoverflow.com/a/46291979
fac 0 = 1
fac 1 = 1
fac n = n * fac (n-1)

listPermProp :: [Int] -> Property 
listPermProp xs = (len >= 0 && len < 10) ==> fac len == pLen where 
  len = length xs -- Length of list
  pLen = length (perms xs) -- Number of subsets 

main :: IO ()
main = do
    putStrLn "== Testing Excersise 3 =="
    quickCheck listPermProp

---- Reflection
-- Q: Is the property hard to test? If you find that it is, can you given a reason why?
-- A: Again, it is hard to test, even harder then ex 2. Because the complexity for permutations is factorial, which is
-- the worst time complexity we can have. Thus again, the computation scales not well 
-- when n gets over a certain treshold because the number of permutations scales with O(n!).

-- Q: Again, give your thoughts on the following issue: when you perform the test for exercise 5, 
-- what are you testing actually? Are you checking a mathematical fact? Or are you testing 
-- whether perms satisfies a part of its specification? Or are you testing something else still?
-- A: Again, we dont test a mathematical fact. We are only testing a part of the specification of
-- the perms function. More precisely, we are testing whether the property holds for length n >= 0
-- and <= 10.

---- Time spent: Took around 30 minutes to complete 