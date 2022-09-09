module Lab1 where
import Data.List
import Test.QuickCheck
import GHC.Integer (Integer)

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

squares :: [Int]
squares = [ x^2 | x <- [1..] ]

cubes :: [Int]
cubes = [ x^3 | x <- [1..] ]

formula2 :: Int -> Int
formula2 n = n * (n + 1) * (2 * n + 1) `div` 6

formula3 :: Int -> Int
formula3 n = (n * (n + 1) `div` 2) ^ 2

squaresSumEqualsFormula :: Int -> Property
squaresSumEqualsFormula n = n > 0 ==> sum (take n squares) == formula2 n

cubesSumEqualsFormula :: Int -> Property
cubesSumEqualsFormula n = n > 0 ==> sum (take n cubes) == formula3 n

-- Everyone's individual codes: https://github.com/jackvoorham/ssvt-lab1
---- Arie Bisfki:
-- Since this is the first test, I had to figure out how to use QuickCheck.
-- First I had to write a function for creating squares, then the function for the formula
-- and then link them together so to speak.
-- For the cubes I had to do the same but it went quicker because I did not need to spend time on learning QuickCheck.
-- Time spent: 30m
---- Jack Voorham:
-- Time: Took around 30 minutes to complete
---- Kyran van der Laan:
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
---- Chris Gunawardena:
-- The idea is to test the function  by manually adding numbers^2 from 1 to n
-- I googled haskell sum list function https://stackoverflow.com/questions/51279298/haskell-sum-up-the-first-n-elements-of-a-list
-- I spent about 5 hours on this because this is the first time I'm using haskell and took me awhile to understand some stuff

main :: IO ()
main = do
  quickCheck squaresSumEqualsFormula
  quickCheck cubesSumEqualsFormula
