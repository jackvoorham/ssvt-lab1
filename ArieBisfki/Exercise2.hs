module Lab1 where
import Data.List
import Test.QuickCheck  
import GHC.Integer (Integer)

genSubsequences :: Int -> [[Int]]
genSubsequences n = subsequences [1..n]

--20m
prop :: Int -> Property
prop n = n > 0 && n < 20 ==> length (genSubsequences n) == 2^n

main :: IO ()
main = do
  quickCheck prop