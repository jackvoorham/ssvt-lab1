module Lab1 where
import Data.List
import Test.QuickCheck
import GHC.Integer (Integer)

genSubsequences :: Int -> [[Int]]
genSubsequences n = subsequences [1..n]

---- Arie Bisfki
-- Create abstraction over subsequences function so that it can be called more simply.
-- Devise property function.
-- Figure out how I am going to prevent the thing from hanging up...
-- Decided on filtering input. Then decided on a max based on the size of 2^n. Figured that 2^20 is plenty large.
-- A: It's hard to test because the result of 2^n grows so quickly, so after a very early point it starts hanging up.
-- A: I think we're only testing a subset of numbers because of the filter from 1 to 20.
-- Time spent: 20m.

---- Jack Voorham
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

--  Time: Took around 30 minutes to complete

---- Kyran van der Laan
-- My thoughts:
-- For some reason the length needs to be outputted in Int not Integer (this realisation took some time).
-- I need to learn the notation better since subLength2 occupied a lot of time because I put a wrong declaration.
-- Initial quickcheck gives very slow execution, this is because the powerset become too large to calculate above a certain threshold.
-- However, this is not necessary for the properties.
-- The property we need to test is whether the statement holds for natural numbers.
-- I will do this by using the subsequences function and calculating the length of the original list and the length of the powerset.
-- Q: Is the property hard to test? If you find that it is, can you given a reason why?
-- A: Yes I find this property hard to test because above a length of 25 the powerset and it's length become very large.
--    This means that calculating 2 to the power of this is very slow. This also means that you cannot test all natural numbers using
--    quickcheck.
-- Q: When you perform the test for exercise 4, what are you testing actually? Are you checking a mathematical fact?
--    Or are you testing whether subsequences satisfies a part of its specification? Or are you testing something else still?
-- A: I believe we are actually testing a subset of natural numbers, namely the natural number 1 until 25. This means we aren't
--    checking a mathematical fact because we cannot draw a conclusion about all natural numbers from only a small subset.
--    I think that the only thing we can say for certain is that the property holds for all natural number between 1 and 25 meaning
--    we are only testing part of the specification.
-- Time spend: roughly 1 hour for me.
prop :: Int -> Property
prop n = n > 0 && n < 20 ==> length (genSubsequences n) == 2^n

main :: IO ()
main = do
  quickCheck prop
