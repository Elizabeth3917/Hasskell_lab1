{- #1 Подвоєння парних чисел
Написати функцію doubleEven, яка для списку цілих чисел подвоює тільки парні числа, залишаючи непарні без змін.
-}
doubleEven :: [Int] -> [Int]
doubleEven xs = [if even x then 2 * x else x | x <- xs]

{- #2 Сума непарних чисел
Написати функцію sumOdd, яка обчислює суму всіх непарних чисел у списку.
-}
sumOdd :: [Int] -> Int
sumOdd xs = sum[x | x <- xs, odd x]

{- #3 Абсолютні значення
Написати функцію absList, яка переводить усі числа у списку до їх абсолютних значень 
-}
absList :: (Num a, Ord a) => [a] -> [a]
absList xs = [if x < 0 then -x else x | x <- xs]

{- #4 Рекурсивна довжина списку
Реалізувати функцію myLength, яка рахує довжину списку
без використання вбудованої функції length.
-}
myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = 1 + myLength xs

{- #5 Рекурсивне перевертання списку
Реалізувати функцію myReverse, яка рекурсивно перевертає список.
-}
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

{- #6 Максимум списку
Реалізувати функцію myMaximum, яка знаходить найбільший елемент у 
списку (без використання maximum).
-}
myMaximum :: Ord a => [a] -> a
myMaximum [x] = x
myMaximum (x:xs) = max x (myMaximum xs)

{- #7 Генерація піфагорових трійок
Використати спискові вирази, щоб знайти всі піфагорові 
трійки (a,b,c) для 1 <= a,b,c <= 20.
-}
pythagoreanTriples :: [(Int, Int, Int)]
pythagoreanTriples = [(a,b,c) | a <- [1..20], b <- [1..20], c <- [1..20], a^2 + b^2 == c^2]

{- #8 Числа Фібоначчі
Рекурсивно реалізувати функцію обчислення n-го числа Фібоначчі.
-}
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

main :: IO ()
main = do
    putStrLn "Подвоєння парних чисел"
    print (doubleEven [1,2,3,4])
    
    putStrLn "Сума непарних чисел"
    print (sumOdd [1,2,3,4,5])
    
    putStrLn "Абсолютні значення"
    print (absList [-3,5,-7, 0.5])
    
    putStrLn "Рекурсивна довжина списку"
    print (myLength [1,2,3,4])
    
    putStrLn "Рекурсивне перевертання списку"
    print (myReverse [1,2,3])
    
    putStrLn "Максимум списку"
    print (myMaximum [3,7,2,9,4])
    
    putStrLn "Генерація піфагорових трійок"
    print (take 5 pythagoreanTriples)
    
    putStrLn "Числа Фібоначчі 10"
    print (fib 10)
 
