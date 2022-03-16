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