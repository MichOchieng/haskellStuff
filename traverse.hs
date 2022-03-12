{-
Traversables

-}
 
instance Traversable Maybe where
     traverse f Nothing = pure Nothing
     traverse f (Just x) = Just <*> f x

-- f: a -> h b where h is an Applicative
-- (Just x): some traversable
-- Just <*> f x: h(t b)
--     Also equal to: fmap Just (f x)

instance Traversable WeirdList where
    traverse f (Single x) = Single <*> f x
    traverse f (Pair x y) = Pair <*> f x <*> f y
    traverse f (Many xs)  = Mnay <*> traverse (traverse f ) xs