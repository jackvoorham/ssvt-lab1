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


---- Excersise 1 for assignment

oneLhs :: Integer -> Integer 
oneLhs n = sum [k^2 | k <- [1..n]]

oneRhs :: Integer -> Integer
oneRhs n = div (n*(n+1)*(2*n+1)) 6 

testOneEquality :: Integer -> Bool
testOneEquality n = let x = abs n in oneLhs x == oneRhs x 

twoLhs :: Integer -> Integer 
twoLhs n = sum [k^3 | k <- [1..n]]

twoRhs :: Integer -> Integer
twoRhs n = div (n * (n + 1)) 2 ^ 2

testTwoEquality :: Integer -> Bool
testTwoEquality n = let x = abs n in twoLhs x == twoRhs x 

main :: IO ()
main = do
    putStrLn "== Testing Excersise 1.1 =="
    quickCheck testOneEquality
    putStrLn "== Testing Excersise 1.2 =="
    quickCheck testTwoEquality
    

---- Time: Took around 30 minutes to complete
