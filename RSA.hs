import Prelude hiding (gcd)
import System.Random

-- Euclid's algorithm for GCD
gcd :: Integer -> Integer -> Integer
gcd a 0 = a
gcd a b = gcd b (a `mod` b)


-- Decomposition of n-1 into s and d for Rabin-Miller (n-1 = 2^s * d)
primeDecomp :: Integer -> Integer -> (Integer, Integer)
primeDecomp n s
    | n `mod` 2 == 1 = if s == 0 then primeDecomp (n-1) s else (s, n) 
    | otherwise = primeDecomp (n `div` 2) (s+1)

-- Hint from powMod at https://wiki.haskell.org/Testing_primality#Miller-Rabin_Primality_Test
fastRecSquareMod :: Integer -> Integer -> Integer -> Integer
fastRecSquareMod _ 0 _ = 1
fastRecSquareMod x n m
    | even n = fastRecSquareMod (x^2 `rem` m) (n `div` 2) m
    | otherwise = (x * fastRecSquareMod (x^2 `rem` m) ((n-1) `div` 2) m) `mod` m


-- Rabin-Miller Monte Carlo primality test
rmPrimeTest :: Integer -> Integer -> Bool
rmPrimeTest n k =
    let (s, d) = primeDecomp n 0
        darts :: Integer -> Bool
        darts 0 = True
        darts k = let
            g = mkStdGen (fromInteger (k*n+k*s*d) :: Int)
            a = fst $ randomR (2, n-2) g 
            x = fastRecSquareMod a d n
            score :: Integer -> Integer -> Bool
            score s x
                | s == 0 = y == 1
                | y == 1 && x /= 1 && x /= n-1 = False
                | otherwise = score (s-1) y
                where y = x `mod` n * x `mod` n
            in if score s x then darts (k-1) else False
    in darts k
