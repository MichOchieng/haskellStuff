{-
CPS
    Procedures aren't allowed to return to the caller
        - Can use callback functons provided by the caller

-}

-- Ex 1

-- Insert a number into a list
-- If list empty insert into list
-- If head of list less than incoming num prepend
--  otherwise append
insert :: [Int] -> Int -> [Int]
insert [] y     = [y]
insert (x:xs) y =
    if x > y
        then y:x:xs
        else x:insert xs y

cpsInsert :: [Int] -> Int -> ([Int] -> r) -> r
cpsInsert [] y k = k [y]
cpsInsert (x:xs) y k =
    if x > y
        then k (y:x:xs)
        else cpsInsert xs y $ \res -> k (x:res)

-- Ex 2 Length

lstLength :: [a] -> Int
lstLength = foldr (\x acc -> acc + 1) 0

cpsLength :: [a] -> Int -> Int
cpsLength [] acc      = acc
cpsLength (x:xs) acc  = cpsLength xs (acc + 1)

add :: Int -> Int -> Int
add x y = x + y

addC :: Int -> Int -> (Int -> r) -> r
addC x y k = k (add x y)

myTake :: Int -> [a] -> [a]
myTake 0 _  = []
myTake _ [] = []
myTake i (x:xs) = x : myTake (i-1) xs

takeK :: ([a] -> p) -> Int -> [a] -> p
takeK k 0 _  = k[]
takeK k _ [] = k []
takeK k n (x:xs) = k (x : take (n-1) xs)


main = do
    let test = myTake 3 [1..13]
    print test