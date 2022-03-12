newtype Pair b a = Pair { getPair :: (a,b)} deriving Show

newtype Handle =  Handle { getHandle :: Handle} deriving Show -- Allows for undefined vals to be passed

instance Functor (Pair c) where
    fmap f (Pair (a,b)) = Pair (f a, b)

except' :: Handle -> String 
except' (Handle undefined) = "Error"

main = do
    print $ fmap (+3) (Pair (1,1))
    print $ except' (Handle undefined) 

