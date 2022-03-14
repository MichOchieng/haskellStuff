{-
Foldables

-}

length :: [Int] -> Int
length = foldr (\x -> (+) 1) 0

myMap :: (a -> a) -> [a] -> [a]
myMap f = foldr((:) . f) []


main = do
    print $ foldr (\x y -> (x * y) + x) 1 [1,2,3,4]
    let lst = [1,2,3]
    print $ myMap (+1) lst
