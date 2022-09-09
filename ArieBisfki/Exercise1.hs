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

---- Arie Bisfki:
-- Since this is the first test, I had to figure out how to use QuickCheck.
-- First I had to write a function for creating squares, then the function for the formula
-- and then link them together so to speak.
-- For the cubes I had to do the same but it went quicker because I did not need to spend time on learning QuickCheck.
-- Time spent: 30m
main :: IO ()
main = do
  quickCheck squaresSumEqualsFormula
  quickCheck cubesSumEqualsFormula
