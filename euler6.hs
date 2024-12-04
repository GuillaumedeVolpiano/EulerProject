sumOfNumbers :: Int -> Int
sumOfNumbers x = div (x * (x+1)) 2 

sumOfSquares :: Int -> Int
sumOfSquares x = div (x * (x + 1) * (2 * x + 1)) 6

main = putStrLn . show $ ((sumOfNumbers 100)^2 - sumOfSquares 100)
