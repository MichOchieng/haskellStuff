{-
Monoids
    Associative binary function and a value which acts as an identity with respect to that function

    (1) Function takes two params
    (2) Params and the returned val have the same type
    (3) There exists such a val that doesnt change other values when used with the binary function
-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

class Monoid m where
    mempty  :: m
    mappend :: m -> m -> m
    mconcat :: [m] -> m

data Tree a = EmptyTree 
    | Node a (Tree a) (Tree a)
    deriving (Show)

newtype Product a = Product {getProduct :: a}
    deriving (Show,Eq,Read,Bounded,Ord)

newtype First a = First {getFirst :: Maybe a}
    deriving (Eq,Ord,Read,Show)

instance Semigroup (First a) => Prelude.Monoid (First a) where
    mempty = First Nothing
    First (Just x) `mappend` _ = First (Just x)
    First Nothing `mappend` x  = x

instance Foldable Tree where
    foldMap f EmptyTree = Prelude.mempty 
    foldMap f (Node a left right) = foldMap f left `Prelude.mappend` 
                                    f a            `Prelude.mappend` 
                                    foldMap f right

-- instance Monoid a => Monoid (Maybe a) where
--     mempty = Nothing 
--     Nothing `mappend` m = m
--     m `mappend` Nothing = m
--     Just m1 `mappend` Just m2 = Just (m1 `mappend` m2)

test :: (First a -> Maybe a) -> First a -> Maybe a
test f (First a) = f $ First a

myTree :: Tree Char
myTree =
    Node 'A' 
        (Node 'B' 
            (Node 'D' EmptyTree EmptyTree) 
            (Node 'E' EmptyTree EmptyTree)   
        ) 
        (Node 'C' 
            (Node 'F' EmptyTree EmptyTree) 
            (Node 'G' EmptyTree EmptyTree)
        )

main = do
    print $ getFirst $ First (Just 13)
    print $ "im a " `Prelude.mappend` "monoid"


