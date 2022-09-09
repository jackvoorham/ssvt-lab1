module Lab1 where
import Data.List
import Test.QuickCheck  
import GHC.Integer (Integer)

prime :: Integer -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
  where xs = takeWhile (\ y -> y^2 <= n) primes

primes :: [Integer]
primes = 2 : filter prime [3..] 

reversal :: Integer -> Integer
reversal = read . reverse . show

prop :: Integer -> Bool
prop x = prime x && prime (reversal x)

-- 20m
-- gather reusable functions
-- compose final function
-- the word stream and the lack of arguments indicated that the function needed to be an infinite list
-- in my mind I was also already thinking about filter because we accept only some of the numbers between 1 and 10000,
-- namely on the condition that they are prime and that their reversal is also prime
-- Lost some time on testing the reversibleStream
reversibleStream :: [Integer] 
reversibleStream = filter prop [1..10000]

propTest :: Int -> Property
propTest n = n > 0 && n < reversiblePrimesAmount ==> prop (reversibleStream !! n) where 
    reversiblePrimesAmount = 260 -- hardcoded based on result of length reversibleStream

main :: IO ()
main = do
  quickCheck propTest