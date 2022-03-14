{-
Monads

    Allows you to take a value with the context (m a) and apply it to a function that takes a normal unwrapped value

    (>>=) :: (Monad m) => m a -> (a -> m b) -> m b
-}
data Maybe' a = Just' a
            | Nothing'
            deriving Show

instance Functor Maybe' where
    fmap f Nothing'      =  Nothing'
    fmap f (Just' x)     = Just' $ f x

class (Functor f) => Applicative f where
    pure  :: a -> f a -- f is the Applicative here
    -- A beffier fmap that can take a functor with a function along w another functor
    (<*>) :: f (a -> b ) -> f a -> f b

class Monad m where  
    -- Basically how pure works with applicatives, returns a monadic val
    return :: a -> m a  
    -- Binds a monadic value to a regular function and returns a monadic value
    (>>=) :: m a -> (a -> m b) -> m b  
  
    (>>) :: m a -> m b -> m b  
    x >> y = x Main.>>= \_ -> y  
  
    fail :: String -> m a  
    fail msg = error msg  

instance Main.Applicative Maybe' where
    pure           = Just'
    Nothing' <*> _ = Nothing'
    (Just' f) <*> something = fmap f something

instance Main.Monad Maybe' where
    return x       = Just' x
    Nothing' >>= f = Nothing'
    Just' x  >>= f = f x
    fail _         = Nothing'

functorTest :: Maybe' [Char] -> Maybe' [Char]
functorTest = fmap (++" ochieng")

main = do
    let val = Just' "Mich"
    print $ functorTest val
    print $ (\x y -> (x+1) * y) <$> Just 3 Prelude.<*> Just pi 
    print $ Just' "Mich" Main.>>= (\x -> Main.return $ x ++ " Ochieng Monad")
    