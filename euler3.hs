main = putStrLn . show . maximum . primeFactors $ 600851475143

primeFactors :: Int -> [Int]
primeFactors x = filter isRelativePrime factors
  where 
        factors = filter (isFactor x) [2..sqx]
        sqx = floor . sqrt . fromIntegral $ x
        isFactor v f = v `mod` f == 0
        isRelativePrime v = not . any (\p -> v /= p && isFactor v p) $ factors
