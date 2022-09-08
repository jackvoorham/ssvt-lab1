import Test.QuickCheck

---- Excersise 1 for assignment

-- Excersise 2 from the workshop testing and formal methods

-- First property: Check for equality of left and right side

oneLhs :: Integer -> Integer
oneLhs n = sum [k^2 | k <- [1..n]]

oneRhs :: Integer -> Integer
oneRhs n = div (n*(n+1)*(2*n+1)) 6 

testOneEquality :: Integer -> Bool
testOneEquality n = let x = abs n in oneLhs x == oneRhs x 

twoLhs :: Integer -> Integer 
twoLhs n = sum [k^3 | k <- [1..n]]

twoRhs :: Integer -> Integer
twoRhs n = (div (n*(n+1)) 2)^2

testTwoEquality :: Integer -> Bool
testTwoEquality n = let x = abs n in twoLhs x == twoRhs x 

main :: IO ()
main = do
    quickCheck testOneEquality
    quickCheck testTwoEquality