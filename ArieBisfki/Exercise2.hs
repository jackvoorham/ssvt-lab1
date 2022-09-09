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
prop :: Int -> Property
prop n = n > 0 && n < 20 ==> length (genSubsequences n) == 2^n

main :: IO ()
main = do
  quickCheck prop
