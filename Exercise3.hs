module Lab1 where
import Data.List
import Test.QuickCheck
import GHC.Integer (Integer)

perms :: [a] ->[[a]]
perms [] = [[]]
perms (x:xs) = concat (map (insrt x) (perms xs)) where
    insrt x [] = [[x]]
    insrt x (y:ys) = (x:y:ys) : map (y:) (insrt x ys)

perms' :: Int -> [[Int]]
perms' n = perms [1..n]

factorial :: Int -> Int
factorial n = product [1..n]

prop :: Int -> Property
prop n = n > 0 && n < 10 ==> length (perms' n) == factorial n

---- Arie Bisfki
-- Need to think up a formula for the number of permutations of a list of (n) distinct objects...
-- if n = 1 then 1
-- if n = 2 then 2
-- if n = 3 then 6
-- if n = 4 then 24
-- ...clearly the formula is n!
-- finding suitable range by manually trying the prop function with different arguments to see what the maximum is before it starts hanging up.
-- A: It's hard to test because n! grows very fast, so after a very early point it starts hanging up.
-- A: I think we're only testing a subset of numbers because of the filter from 1 to 10.
-- Time spent: 45m

---- Jack Voorham
-- Reflection
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
-- Time spent: Took around 30 minutes to complete

---- Kyran van der Laan
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
main :: IO ()
main = do
    quickCheck prop
