module Lab1 where
import Data.List
import Test.QuickCheck  
import GHC.Integer (Integer)

genSubsequences :: Int -> [[Int]]
genSubsequences n = subsequences [1..n]

--20m
-- create abstraction over subsequences function so that it can be called more simply
-- devise property function
-- figure out how I am going to prevent the thing from hanging up...
-- decided on filtering input. Then decided on a max based on the size of 2^n. Figured that 2^20 is plenty large.
prop :: Int -> Property
prop n = n > 0 && n < 20 ==> length (genSubsequences n) == 2^n

main :: IO ()
main = do
  quickCheck prop