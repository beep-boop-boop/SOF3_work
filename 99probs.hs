mylast :: [a] -> a
mylast [] = undefined
mylast [x] = x
mylast (x : xs) = mylast xs

myButLast :: [a] -> a
myButLast [] = undefined 
myButLast [x1, x2] = x1
myButLast (x : xs) = myButLast xs

myLength :: [a] -> Int
myLength = foldr (\_ count -> count + 1) 0

compress :: Eq a => [a] -> [a]
compress [] = []
compress [x] = [x]
compress (x1 : x2 : xs) = 
    if x1 == x2 
    then compress xs 
    else x1 : compress xs

encode :: Eq a => [a] -> [(Int, a)]
encode xs = [(count element xs, element) | element <- xs] where
    count item = foldr (\x count -> if x == item then count + 1 else count + 0) 0


dupli :: [a] -> [a]
dupli [] = []
dupli (x : xs) = x : x : dupli xs


slice :: [a] -> Int -> Int -> [a]
slice [] _ _ = []
slice _ _ 0 = []
slice (x : xs) 1 finish = x : slice xs 1 (finish - 1)
slice (_ : xs) start finish = slice xs (start - 1) (finish - 1)

slice' :: [a] -> Int -> Int -> [a]
slice' xs start finish = take (finish - start + 1) (drop (start - 1) xs)


range :: Int -> Int -> [Int]
range start finish | start <= finish = start : range (start + 1) finish
                   | otherwise = []

isPrime :: Int -> Bool
isPrime x = all (== True) [x `mod` y /= 0 | y <- [2 .. x - 1]]

myGCD :: Int -> Int -> Int 
myGCD a b | a == b = a
          | a > b = myGCD (a - b) b
          | a < b = myGCD a (b - a)

pack :: Eq a => [a] -> [[a]]
pack [] = []
pack [x] = [[x]]
pack (x1 : x2 : xs) | x1 == x2 = (x1 : x2)++ pack xs
                    | otherwise = [x1] ++ pack (x2 : xs)