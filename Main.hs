-- #1
doubleEven :: [Int] -> [Int]
doubleEven xs = [If even x then 2 * x else x | x <- xs]

-- #2
sunOdd :: [Int] -> Int
sumOdd xs = sum[x | x <- xs, odd x]

-- #3
absList :: [Int] -> [Int]
absList xs = [if x < 0 then -x else x | x <- xs]

-- #4
myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = 1 + myLength xs

-- #5
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

-- #6
myMaximum :: Ord a => [a] -> a
myMaximum [x] = x
myMaximum (x:xs) = max x (myMaximum xs)

-- #7
pythagoreanTriples :: [(Int, Int, Int)]
pythagoreanTriples = [(a,b,c) | a <- [1..20], b <- [1..20], c <- [1..20], a^2 + b^2 == c^2]

-- #8
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fid (n-2)
