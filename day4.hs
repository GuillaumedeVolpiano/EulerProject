-- Assume that there is at least one palindromic number that is the product of two numbers bigger than 900
hundredsPalindromic :: [Int]
hundredsPalindromic = [x * y | x <- [900..999], y <- [900..999], isPalindromic (x*y)]
  where
    isPalindromic x = show x == reverse (show x)

main = putStrLn . show . maximum $ hundredsPalindromic
