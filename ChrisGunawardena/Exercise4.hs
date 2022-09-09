

-- The natural number 13 has the property that it is prime and its reversal, the number 31, is also prime. Write a function that finds all primes < 10000 with this property. Follow this type declaration:


prime :: Integer -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs   where xs = takeWhile (\ y -> y^2 <= n) primes
primes :: [Integer]
primes = 2 : filter prime [3..1000] 



reversal :: Integer -> Integer
reversal = read . reverse . show

reversibleStream :: [Integer] 

reversibleStream n = filter (\x -> (isPrime x && (isPrime reversal x))) primes



-- The was to use filter to filter all primes by the function reversal and check if its also a prime.


-- maybe we can test this by getting a list of primes and checking for reversal primes in this list