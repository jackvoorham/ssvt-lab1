-- Base code imported from Lab 1 at https://canvas.uva.nl/courses/32483/files/7147680?wrap=1&fd_cookie_set=1

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

-- My thoughts:
-- We need to find the number of permutations of a given list.
-- My first thought is using length in combination with the permutations command.
-- The formula we came up with is that the number of permutations in a list equals n!.
-- This means that the number of permuations = factorial the number of items in the original list.
-- We test this by creating the factorial and using the permutations command and seeing if they are equal.
-- We run into the problem that negative numbers don't seem to generate a nice list when starting at 1.
-- We solved this by adding a zero to the first list and the factorial needs to be taken from the list
-- 1 till abs of n + 2 since -1 would create the list [1,0,-1] which is three numbers and each subsequent number
-- would add one more element.
-- Q: Is the property hard to test? If you find that it is, can you given a reason why?
-- A: Yes this property is again hard to test for the same reason as Exercise 2. The amount of permutations become
--    increasingly too large to test in reasonable time.
-- Q: When you perform the test for exercise 5, what are you testing actually? Are you checking a mathematical fact? 
--    Or are you testing whether perms satisfies a part of its specification? Or are you testing something else still?
-- A: We are only testing whether perms satisfies part of the specification since only the lists [1..-7] and [1..7]
--    can be tested in reasonable time.
-- Time spend: 1.5 hours.

permLength :: [Integer] -> Int 
permLength n = length (permutations n)

listFact :: [Integer] -> Integer
listFact [] = 0
listFact n = foldl (*) (head (n)) (tail n)

testProp :: Integer -> Property
testProp n = (n >= -7 && n <= 7) ==> if n > 0 
                                      then toInteger (permLength [1..n]) == listFact [1..n]
                                      else toInteger (permLength [1, 0.. n]) == listFact [1..(abs(n) + 2)]

main :: IO ()
main = do
  putStrLn "== Check test =="
  quickCheck testProp