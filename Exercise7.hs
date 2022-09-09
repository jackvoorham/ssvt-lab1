module Exercise7 where
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

-- Comments:
---- Kyran van der Laan (General comments):
-- In order to implement the exercise, the luhn algorithm must be implemented.
-- This can then be used to calculate the check digit, if this equals the original check digit the number is valid.
-- This can be done by doubling every other digit from right to left and calculating the sum as described in the wiki-page:
-- https://en.wikipedia.org/wiki/Luhn_algorithm#Description
-- In order to check from what company a card is, we can use the descriptions found at wikipedia:
-- https://en.wikipedia.org/wiki/Payment_card_number
-- Here we can see that American Express uses 34 and 37 for the first two numbers, Master uses 51–55 and Visa uses 4.
-- Along with this the cards are 15, 16 and 13 or 16 digits in length respectively.
-- We can use this to check if a card is from a certain company alongside with the made luhn algorithm checker.
-- A test of correctness can be made by using quickcheck along with known valid and invalid card number to very if everything works as intended.
-- Time spend: 15 minutes (for the explanation)

---- Jack Voorham (Attempted code, comments are based on that):
-- To finish the assignment i would have continued with implmenting the functions for luhns
-- algorithm. Specifically, the function that checks whether a value in the list is greater 
-- then 9 and add the digits of the double digit number and replace it with the original value.
-- to implement this function we again can use the digs function (to convert double digit, to
-- a array) and then take the sum, with the built in sum function. 
-- We also need to implement the luhn :: integer -> Bool function. This can be constructed by
-- zipping multiple functions. First we need to convert the number to a list with the digs function,
-- then we call doubleEvenEntries, then we call the function that converts the double digits to 
-- a single digit by adding the two digits. Then finally, we can check if the summation of the 
-- digits in the modified list have a sum that has 0 as its modulo 10 output. 
    
-- To finish the mastercard, visa etc. specifications we can look up the properties of certain 
-- credit cards companies (e.g length, starting digits) and write functions that checks all properties
-- and returns true if the properties indeed hold on the given number and also check if luhns algorithm
-- holds by calling the luhns function. 
-- Time taken: 30 minutes, Not enough time to finish

-- Chris Gunawardena:
-- No time left

-- Arie Bisfki:
-- No time left

--------------------------------------------------------------------

-- Convert a number to a array with digits 
-- From https://stackoverflow.com/a/3963286
digs :: Integral x => x -> [x]
digs 0 = []
digs x = digs (x `div` 10) ++ [x `mod` 10]

-- Doubles every even entry in a list of integers
doubleEvenEntries :: [Int] -> [Int]
doubleEvenEntries xs = x : y*2 : doubleEvenEntries xs


