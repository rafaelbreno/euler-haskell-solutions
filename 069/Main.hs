import Text.Read.Lex (numberToFixed)
import Data.List (nub, sortBy, maximumBy)
-- isPrime checks if a value is a Prime.
isPrime :: Int -> Bool
isPrime n 
  | n <= 1 = False
  | otherwise = all (\x -> n `mod` x /= 0) [2..(floor . sqrt $ fromIntegral n)]

-- primes represents an infinite list of prime numbers
primes :: [Int]
primes = 2 : filter isPrime [3,5..]

-- prime factors of a number 
primeFactors :: Int -> [Int]
primeFactors n = factors n primes where
  factors n (x:xs)
    | x * x > n       = [n]
    | n `mod` x == 0  = x : factors (n `div` x) (x:xs)
    | otherwise       = factors n xs

phi :: Int -> Int
phi n = round $ fromIntegral n * product [1 - 1 / fromIntegral p | p <- nub (primeFactors n)]

-- calculates the n / phi(n)
totientMaximum :: Int -> Float
totientMaximum n = fromIntegral n / fromIntegral (phi n)


-- basically returns a tuple (n, n / phi(n))
pairTotientMax :: Int -> (Int, Float)
pairTotientMax n = (n, totientMaximum n)

-- map through [2..n], calling `pairTotientMax`
-- filter the max value, getting a (Int, Float)
-- fst gets the first item (a,_) from the tuple.
findMaximun :: Int -> Int
findMaximun n = fst 
    $ maximumBy (\(_, a) (_, b) -> compare a b)
    $ map pairTotientMax [2..n]

main :: IO ()
main = do
  let limit = 1_000_000
  print $ "phi(4) = " ++ show (phi 4) 
  print $ "phi(10) = " ++ show (phi 10) 
  print $ "totientMaximum(6) = " ++ show (totientMaximum 6) 
  print $ "totientMaximum(7) = " ++ show (totientMaximum 7) 
  print $ "pairTotientMax(6) = " ++ show (pairTotientMax 6) 
  print $ "pairTotientMax(7) = " ++ show (pairTotientMax 6) 
  print $ "findMaximun(limit) = " ++ show (findMaximun limit)

todo :: a 
-- Todo implements a similar `todo!()` from Rust.
todo = case undefined of {}
