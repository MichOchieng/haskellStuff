import Prelude hiding (Applicative)
import Control.Applicative (ZipList (ZipList))
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

instance Applicative [] where
    pure x = [x]
    fs <*> xs = [f x | f <- fs, x <- xs]

instance Applicative IO where
    pure = return
    a <*> b = do
        f <- a
        f <$> b

instance Applicative ZipList where
    pure x = ZipList (repeat x)
    ZipList fs <*> ZipList xs = ZipList (zipWith (\f x -> f x) fs xs)

con :: IO String
con = (++) <$> getLine Prelude.<*> getLine 
{-
    (1) partially applies ++ to the first getLine
    (2) map the partial function application to the second getLine
    (3) return the concat' of the two getLines
-}

main = do
    print $ (++) <$> Just "Happy " Prelude.<*> Just "Birthday"
    let funcs = [(+2),(*2)]
    let lst   = [1,2]
    print $ ZipList funcs Prelude.<*> ZipList lst

