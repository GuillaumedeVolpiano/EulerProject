import Data.List (unfoldr)

main = putStrLn . show . sum . unfoldr fib $ (0,1)

fib :: (Int, Int) -> Maybe (Int, (Int, Int))
fib (x, y) 
  | x + y >= 4 * 10^6 = Nothing
  | even (x + y) = Just (x + y, (y, x + y))
  | otherwise = fib (y, x + y)
