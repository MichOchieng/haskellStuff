{-
Monoids
    Associative binary function and a value which acts as an identity with respect to that function
    In other words a semi group with an identity argument

    (1) Function takes two params
    (2) Params and the returned val have the same type
    (3) There exists such a val that doesnt change other values when used with the binary function
-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

import qualified Data.Foldable as F
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
    

instance F.Foldable Tree where
    foldMap f EmptyTree = Prelude.mempty 
    foldMap f (Node a left right) = foldMap f left `Prelude.mappend` 
                                    f a            `Prelude.mappend` 
                                    foldMap f right

-- instance Monoid a => Monoid (Maybe a) where
--     mempty = Nothing 
--     Nothing `mappend` m = m
--     m `mappend` Nothing = m
--     Just m1 `mappend` Just m2 = Just (m1 `mappend` m2)

myTree :: Tree Int
myTree =
    Node 1 
        (Node 2 
            (Node 4 EmptyTree EmptyTree) 
            (Node 5 EmptyTree EmptyTree)   
        ) 
        (Node 3 
            (Node 6 EmptyTree EmptyTree) 
            (Node 7 EmptyTree EmptyTree)
        )

main = do
    print $ foldl (+) 0 myTree


