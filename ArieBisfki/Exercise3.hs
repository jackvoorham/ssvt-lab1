module Lab1 where
import Data.List
import Test.QuickCheck  
import GHC.Integer (Integer)

-- need to think up a formula for the number of permutations of a list of (n) distinct objects...
-- if n = 1 then 1
-- if n = 2 then 2
-- if n = 3 then 6
-- if n = 4 then 24
-- ...clearly the formula is n!
-- finding suitable range by manually trying the prop function with different arguments to see what the maximum is before it starts hanging up.
-- 45m

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

main :: IO ()
main = do
    quickCheck prop