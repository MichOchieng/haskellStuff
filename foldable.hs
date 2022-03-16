{-
Foldables

    foldr :: (\elem acc -> term) acc0 list
    foldl :: (\acc elem -> term) acc0 list

-}

length :: [Int] -> Int
length = foldr (\x -> (+) 1) 0

myMap :: (a -> a) -> [a] -> [a]
myMap f = foldr((:) . f) []

{-
Exercises
    (1)
        Reverse list with one fold
    (2)
        Return all prefixes of a given list
    (3)

    (4)

-}

rev :: [a] -> [a]
rev = foldl (\acc x -> x:acc) []
-- f could also be `flip (:)` but this isn't as clear

prefixes :: [a] -> [[a]]
prefixes = foldr (\x acc -> [x] : (map ((:) x) acc)) []

-- prefixHelper :: [a] -> [a]
-- prefixHelper []     = []
-- prefixHelper (x:xs) = prefixHelper $ init (x:xs)

main = do
    print $ foldr (\x y -> (x * y) + x) 1 [1,2,3,4]
    let lst = [1,2,3]
    print $ myMap (+1) lst
