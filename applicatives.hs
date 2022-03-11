{-
Applicatives
    Allows us to map functions inside of functors over other functors
    Constraints:
        To be an Applicative you must first be a functor (can use fmap)
-}

class (Functor f) => Applicative f where
    pure  :: a -> f a -- f is the Applicative here
    -- A beffier fmap that can take a functor with a function along w another functor
    (<*>) :: f (a -> b ) -> f a -> f b

instance Applicative Maybe where
    pure          = Just
    Nothing <*> _ = Nothing    
    (Just f ) <*> something = fmap f something

test ::(Show a) => a -> Maybe a
test a = Prelude.pure (*a) Prelude.<*> Just a 

main = do
    print $ test 3