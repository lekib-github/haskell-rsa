import System.Random
import Data.Char
import System.Environment


-- Extended Euclid's algorithm for computing the multiplicative inverse, d, of e (mod t).
-- Only for coprime e and t!! - d ≡ e^(−1) (mod λ(n))
exEuc :: Integer -> Integer -> Integer
exEuc e t =
    let process :: Integer -> Integer -> Integer -> Integer -> Integer
        process d _ t' 0
            | t' > 1 = error "e noninvertible!!"
            | d < 0 = d + t
            | otherwise = d
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
mrPrimeTest :: Integer -> Int -> StdGen -> Bool
mrPrimeTest n k g =
    let -- Decomposition of n-1 into s and d for Miller-Rabin (n-1 = 2^s * d)
        primeDecomp :: Integer -> Integer -> (Integer, Integer)
        primeDecomp n s
            | n `mod` 2 == 1 = if s == 0 then primeDecomp (n-1) s else (s, n) 
            | otherwise = primeDecomp (n `div` 2) (s+1)
        (s, d) = primeDecomp n 0
        darts = take k (randomRs (2, n-2) g)
        throw :: Integer -> Bool
        throw dart = 
            let x = fastRecSquareMod dart d n
                score :: Integer -> Integer -> Bool
                score s x
                    | s == 0 = y == 1
                    | y == 1 && x /= 1 && x /= n-1 = False
                    | otherwise = score (s-1) y
                    where y = x `mod` n * x `mod` n
            in score s x
    in all throw darts

-- RSA key generation - (e, n) public key, (d, n) private key
keygen :: Integer -> StdGen -> (Integer, Integer, Integer)
keygen len g = 
    let lenDiv2 = len `div` 2
        lenMod2 = len `mod` 2
        (p, q) = (head [n | n <- randomRs (2^lenDiv2, 2^(lenDiv2 + 1)) g, mrPrimeTest n 10 g],
                  head (tail [n | n <- randomRs (2^(lenDiv2 + lenMod2), 2^(lenDiv2 + lenMod2 + 1)) g, 
                  mrPrimeTest n 10 g]))
        n = p * q
        -- Carmichael's totient function FOR PRIME FACTORS of n:
        -- n = pq, λ(n) = lcm(λ(p), λ(q)), and since p and q are prime, λ(p) = φ(p) = p − 1, 
        -- and likewise λ(q) = q − 1. Hence λ(n) = lcm(p − 1, q − 1). 
        ctf :: Integer -> Integer -> Integer
        ctf p q = ((p - 1) * (q - 1)) `div` gcd (p - 1) (q - 1) -- lcm(p-1, q-1)
        totient_n = ctf p q
        e = 2^16 + 1
        d = exEuc e totient_n
    in (e, n, d)


asciiToNum :: String -> Integer -> Integer
asciiToNum "" n = n
asciiToNum (c : str) n = asciiToNum str (toInteger (ord c) + n * 256)

numToAscii :: Integer -> String
numToAscii n = 
    let accumHelp ::Integer -> String
        accumHelp 0 = ""
        accumHelp n = chr (fromInteger n `mod` 256) : accumHelp (n `div` 256)
    in reverse (accumHelp n)

encrypt :: (Integer, Integer) -> String -> String
encrypt (exp, key) msg = 
    let bits :: Integer -> Integer
        bits 0 = 0
        bits n = bits (n `div` 2) + 1
        blockN = bits key `div` 8
        process :: String -> String
        process "" = ""
        process msg = 
            let (block, rest) = splitAt (fromInteger blockN) msg
            in show (fastRecSquareMod (asciiToNum block 0) exp key) ++ (' ' : process rest)
    in process msg

decrypt :: (Integer, Integer) -> String -> String
decrypt (exp, key) msg = 
    let blocks = words msg
        process :: [String] -> String
        process [] = ""
        process (b : bs) = numToAscii (fastRecSquareMod (read b :: Integer) exp key) ++ process bs
    in process blocks


main = do 
    let extractKeyAndContent :: String -> String -> IO (Integer, Integer, String)
        extractKeyAndContent keyFile contentFile = do
            keyString <- readFile keyFile
            fileString <- readFile contentFile
            let exp : key : _ = map read (lines keyString)
            return (exp, key, fileString)

    args <- getArgs
    case args of
        ["-keygen", bitL] -> do
            g <- initStdGen
            let (e, n, d) = keygen (read bitL :: Integer) g
                pubFile = "pub.key"
                privFile = "priv.key"
            writeFile pubFile (unlines [show e, show n])
            writeFile privFile (unlines [show d, show n])
            putStrLn "Success."
        ["-encrypt", keyFile, contentFile] -> do 
            (exp, key, contentString) <- extractKeyAndContent keyFile contentFile
            putStrLn (encrypt (exp, key) contentString)
        ["-decrypt", keyFile, contentFile] -> do 
            (exp, key, contentString) <- extractKeyAndContent keyFile contentFile
            putStr (decrypt (exp, key) contentString)
        _ -> do
            putStrLn "Usage:"
            putStrLn "rsa -keygen [n]      Generate keys of bit-length n to files pub.key and priv.key respectively.\n"
            putStrLn "rsa -encrypt [KEY] [FILE]     Encrypt the given file contents with the key to std out. Public key can be used for encryption, private key for signing.\n"
            putStrLn "rsa -decrypt [KEY] [FILE]    Decrypt the given file contents with the key to std out"
