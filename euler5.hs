import Data.Bifunctor (second)
import Data.Function (on)
import Data.List (sortBy, groupBy, maximumBy)
import Data.Ord (comparing)

primes = [2, 3, 5, 7, 11, 13, 17, 19]

decompose :: Int -> Int -> (Int, Int)
decompose number prime
  | number `mod` prime /= 0 = (prime, 0)
  | otherwise = second (+ 1) . decompose (number `div` prime) $ prime
  
primeFact :: [Int]
primeFact = map (uncurry (^) . maximumBy (comparing snd)) . groupBy ((==) `on` fst) . sortBy (comparing fst) . concatMap (\x -> map (decompose x) primes) $ [2..20]
    
main = putStrLn . show . product $ primeFact
