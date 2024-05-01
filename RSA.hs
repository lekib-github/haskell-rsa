import Prelude hiding (gcd)
import System.Random
import Data.Char

-- Euclid's algorithm for GCD
gcd :: Integer -> Integer -> Integer
gcd a 0 = a
gcd a b = gcd b (a `mod` b)

-- Extended Euclid's algorithm for computing the multiplicative inverse, d, of e (mod t).
-- Only for coprime e and t!! - d ≡ e^(−1) (mod λ(n))
exEuc :: Integer -> Integer -> Integer
exEuc e t =
    let process :: Integer -> Integer -> Integer -> Integer -> Integer
        process d _ t' 0 = if t' > 1 then error "e noninvertible!!" else 
                          if d < 0 then d + t else d
        process d d' t e = let q = t `div` e
                           in process d' (d - q * d') e (t - q * e)
    in process 0 1 t e

-- Hint from powMod at https://wiki.haskell.org/Testing_primality#Miller-Rabin_Primality_Test
fastRecSquareMod :: Integer -> Integer -> Integer -> Integer
fastRecSquareMod _ 0 _ = 1
fastRecSquareMod x n m
    | even n = fastRecSquareMod (x^2 `mod` m) (n `div` 2) m
    | otherwise = (x * fastRecSquareMod (x^2 `mod` m) ((n-1) `div` 2) m) `mod` m

-- Miller-Rabin Monte Carlo primality test
mrPrimeTest :: Integer -> Integer -> StdGen -> Bool
mrPrimeTest n k g =
    let -- Decomposition of n-1 into s and d for Miller-Rabin (n-1 = 2^s * d)
        primeDecomp :: Integer -> Integer -> (Integer, Integer)
        primeDecomp n s
            | n `mod` 2 == 1 = if s == 0 then primeDecomp (n-1) s else (s, n) 
            | otherwise = primeDecomp (n `div` 2) (s+1)
        (s, d) = primeDecomp n 0
        darts = randomRs (2, n-2) g
        throw :: Integer -> [Integer] -> Bool
        throw 0 _ = True
        throw k darts = 
            let a = head darts
                x = fastRecSquareMod a d n
                score :: Integer -> Integer -> Bool
                score s x
                    | s == 0 = y == 1
                    | y == 1 && x /= 1 && x /= n-1 = False
                    | otherwise = score (s-1) y
                    where y = x `mod` n * x `mod` n
            in if score s x then throw (k-1) (tail darts) else False
    in throw k darts

-- RSA key generation - (e, n) public key, (d, n) private key
keygen :: Integer -> StdGen -> (Integer, Integer, Integer)
keygen len g = 
    let (p, q) = (head [n | n <- randomRs (2^(len `div` 2), 2^(len `div` 2 + 1)) g, mrPrimeTest n 10 g],
                  head (tail [n | n <- randomRs (2^(len `div` 2 + len `mod` 2), 2^(len `div` 2 + len `mod` 2 + 1)) g, mrPrimeTest n 10 g]))
        n = p * q
        -- Carmichael's totient function FOR PRIME FACTORS of n:
        -- n = pq, λ(n) = lcm(λ(p), λ(q)), and since p and q are prime, λ(p) = φ(p) = p − 1, and likewise λ(q) = q − 1. 
        -- Hence λ(n) = lcm(p − 1, q − 1). 
        ctf :: Integer -> Integer -> Integer
        ctf p q = ((p - 1) * (q - 1)) `div` (gcd (p - 1) (q - 1)) -- lcm(p-1, q-1)
        totient_n = ctf p q
        e = 2^16 + 1
        d = exEuc e totient_n
    in (e, n, d)


asciiToNum :: String -> Integer
asciiToNum "" = 0
asciiToNum (c : str) = (toInteger (ord c)) * 256^(length str) + asciiToNum str

numToAscii :: Integer -> String
numToAscii 0 = ""
numToAscii n = numToAscii (n `div` 256) ++ [chr (fromInteger n `mod` 256)]

encrypt :: (Integer, Integer) -> String -> String
encrypt (exp, key) msg = 
    let bits :: Integer -> Integer
        bits 0 = 0
        bits n = bits (n `div` 2) + 1
        blockN = (bits key)`div` 8
        process :: String -> String
        process "" = ""
        process msg = 
            let split = splitAt (fromInteger blockN) msg
            in show (fastRecSquareMod (asciiToNum (fst split)) exp key) ++ (' ' : process (snd split))
    in process msg


splitOn :: Char -> String -> [String]
splitOn _ "" = []
splitOn delim str =
    let gatherToDelim :: String -> String
        gatherToDelim "" = ""
        gatherToDelim (c : cs)
            | c == delim = ""
            | otherwise = c : gatherToDelim cs
    in gatherToDelim str : splitOn delim (drop (length (gatherToDelim str) + 1) str)

decrypt :: (Integer, Integer) -> String -> String
decrypt (exp, key) msg = 
    let blocks = splitOn ' ' msg
        process :: [String] -> String
        process [] = ""
        process (b : bs) =  numToAscii (fastRecSquareMod (read b :: Integer) exp key) ++ process bs
    in process blocks
    
    
main = do 
    g <- initStdGen
    let (e, n, d) = keygen 2048 g
    print $  decrypt (d, n) (encrypt (e, n) "Vivamus non lacus porttitor, venenatis quam nec, fringilla lectus. Cras vel massa ac ipsum sodales facilisis non sed ante. Proin faucibus, leo a rhoncus semper, velit nisl molestie augue, sed pharetra leo enim sed ligula. Vivamus ac gravida eros. Vivamus nibh tellus, fringilla sed tempor at, congue fringilla justo. Morbi quis volutpat lacus. Morbi posuere consequat fermentum. Proin varius ut justo eu sagittis. Mauris vestibulum mauris quis nisl iaculis, fermentum tempus nunc pharetra. Nunc aliquam efficitur est, id dignissim nunc condimentum in. Curabitur tristique, arcu ac eleifend interdum, quam ex vehicula nibh, vitae faucibus ipsum sapien non eros. Vivamus aliquam lacus et turpis luctus, quis ornare felis iaculis. Vivamus pretium dolor ac leo maximus, in cursus massa.")
