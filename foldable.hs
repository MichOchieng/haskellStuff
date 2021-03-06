import Data.Monoid
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

q :: (a -> Bool) -> (a -> Sum Int)
q p x = if p x
    then Sum 1
    else Sum 0

isEven :: (Integral a) => a -> Bool
isEven = even

countIf :: (a -> Bool) -> Int -> ([a] -> Int)
countIf f acc []     = acc
countIf f acc (x:xs) = if f x
                then countIf f (acc + 1) xs
                else countIf f acc xs

countIf' :: (a -> Bool) -> [Sum Int] -> [a] -> Sum Int
countIf' f lst []     = mconcat lst
countIf' f lst (x:xs) = if f x
                then countIf' f (mappend lst [Sum 1]) xs
                else countIf' f (mappend lst [Sum 0]) xs

toList :: (Foldable t) => (t a) -> [a]
toList = foldMap (\x -> [x])
-- toList = foldMap (:[])

f :: [Int]
f = do 
        x <- [5,6,7]
        y <- [3]
        return (x - y)

m :: Maybe [Int]
m = Just [5,6,7] 

g :: [Int] -> Maybe [Int]
g x = Just (map (subtract 3) x)

main = do
    print $ m >>= g
