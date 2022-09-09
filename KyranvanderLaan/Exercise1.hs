-- Base code imported from Lab 1 at https://canvas.uva.nl/courses/32483/files/7147680?wrap=1&fd_cookie_set=1

module Exercise1 where
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
-- I want to test is whether the statements that 
-- 1^2+2^2+...+n^2 = (n*(n+1)*((2*n)+1))/6 and 1^3+2^3+···+n^3 = ((n*(n+1))/2)^2 hold.
-- I will do this by implementing both sides of the formulas and checking if they are equal with quickcheck.
-- Initially I output a list of integers to confirm the lists with powers are correct.
-- I needed to take node that only natural numbers should be used so 0 and minus ints aren't base cases.  
-- An initial quickCheck failed because the constraint for only natural numbers hasn't been made yet.
-- The inspiration for the starting of the code and the usage of abs for natural numbers came from the 
-- QuickCheck pdf from canvas: https://canvas.uva.nl/courses/32483/files/7077441?module_item_id=1412882.
-- Personally it took some time for me to process how the positive integer generator worked.
-- Tests returned 100 passed tests for both exercises 2 and 3
-- Time spend: 40 minutes.

sumNums2 :: Integer -> Integer
sumNums2 n = sum [k^2 | k <- [1..n]]

sumNums2' :: Integer -> Integer
sumNums2' n = div (n * (n+1) * ((2*n)+1)) 6

sumNums3 :: Integer -> Integer
sumNums3 n = sum [k^3 | k <- [1..n]]

sumNums3' :: Integer -> Integer
sumNums3' n = (div (n*(n+1)) 2)^2

genPositiveIntegers :: Gen Integer
genPositiveIntegers = abs <$> (arbitrary :: Gen Integer) `suchThat` (>0)

testsumNums2 :: Integer -> Bool
testsumNums2 n = sumNums2 n == sumNums2' n

testsumNums3 :: Integer -> Bool
testsumNums3 n = sumNums3 n ==  sumNums3' n

main :: IO ()
main = do
  putStrLn "== Check test =="
  quickCheck $ forAll genPositiveIntegers testsumNums2
  quickCheck $ forAll genPositiveIntegers testsumNums3