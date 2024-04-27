import Prelude hiding (gcd)
import System.Random

-- Euclid's algorithm for GCD
gcd :: Integer -> Integer -> Integer
gcd a 0 = a
gcd a b = gcd b (a `mod` b)


-- Decomposition of n-1 into s and d for Rabin-Miller
primeDecomp :: Integer -> Integer -> (Integer, Integer)
primeDecomp n s
    | n `mod` 2 == 1 = if s == 0 then primeDecomp (n-1) s else (s, n) 
    | otherwise = primeDecomp (n `div` 2) (s+1)

-- Rabin-Miller Monte Carlo primality test
rmPrimeTest :: Integer -> Integer -> Bool
rmPrimeTest n k =
    let (s, d) = primeDecomp n 0
        darts :: Integer -> Bool
        darts 0 = True
        darts k = let
            a = randomRIO (2, n-2) :: IO Integer
            x = a^d `mod` n
            in True
    in darts k
