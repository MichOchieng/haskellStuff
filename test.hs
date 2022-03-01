dub x = x + x
bigDub x y = dub x + dub y
dubs x = if x > 5 
            then x*2
            else x*3

currying :: Int -> Int -> Int
currying = (\x -> (\y -> x + y))
