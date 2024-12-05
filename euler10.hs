primes :: [Int]
primes = 2 : filter (isPrime primes) (3:[2*k + 1 | k <- [2..], mod k 3 == 0 || mod k 3 == 2 ])
  where
    isPrime (p:ps) n = p * p > n || n `rem` p /= 0 && isPrime ps n
    
main = print . sum . takeWhile (<2000000) $ primes
