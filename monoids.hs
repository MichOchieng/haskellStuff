{-
Monoids
    Associative binary function and a value which acts as an identity with respect to that function
-}

class Monoid m where
    mempty  :: m
    mappend :: m -> m -> m
    mconcat :: [m] -> m

