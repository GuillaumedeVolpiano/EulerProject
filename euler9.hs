main = print . head $ [(1000-b-c)*b*c | c <- [0..1000], b <- [0..(1000-c-1)], (1000-b-c)^2+b^2 == c^2]
