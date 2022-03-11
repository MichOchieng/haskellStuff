{-
Traversables

-}

-- instance Traversable Maybe where
--     traverse f Nothing = pure Nothing
--     traverse f (Just x) = Just <*> f x

-- f: a -> h b where h is an Applicative
-- (Just x): some traversable
-- Just <*> f x: h(t b)