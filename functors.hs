{-
Functors
    Can be mapped over lists
    Uses fmap or <*>
        fmap :: (a->b) -> f a -> f b
        Takes a function and a functor then maps that function over the functor
            box = computational context (ex. Maybe or Either)
            ex. Just a such that Just is the box and a is the value in the box
    Laws:
        (1) If we map the id function over a functor the functor that we get back should be the same as the original functor.
        (2) Composing two functions and then mapping the result function over a functor should be the same as 
                first mapping one function over the functor and then mapping the other one.
            i.e fmap (f . g) = fmap f . fmap g || fmap (f . g) F = fmap f (fmap g F)
-}

-- Regular
-- main = do 
--     line <- getLine
--     let line2 = reverse line
--     putStrLn line2

-- fmap version
-- main = do
--     line <- fmap reverse getLine
--     putStrLn line