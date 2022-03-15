import Distribution.Compat.Lens (_1)
import Control.Monad (guard)
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

-- Rope Ex
type Birb = Int
type Pole = (Birb,Birb)

landRight :: Birb -> Pole -> Maybe Pole
landRight n (left,right)
            | abs (left - (right + n)) < 4 = Just (left,right + n)
            | otherwise                    = Nothing

landLeft  :: Birb -> Pole -> Maybe Pole
landLeft  n (left,right)
            | abs ((left + n) - right) < 4 = Just (left + n, right)
            | otherwise                    = Nothing

slipped :: Maybe a -> String
slipped Nothing = "Slipped!"
slipped _        = "Still alright!"

tightRope :: Maybe Pole
tightRope = do
    let start = (0,0)
    step1 <- landLeft 2 start
    step2 <- landRight 3 step1
    landLeft 2 step2

-- Chess example
-- 8x8 grid
type KnightPos = (Int,Int)

knightMovement :: KnightPos -> [KnightPos]
knightMovement (x,y) = do
    -- List of all possible ways to move for the knight 
    (x',y') <- [(x+2,y-1),(x+2,y+1),(x-2,y-1),(x-2,y+1),(x+1,y-2),(x+1,y+2),(x-1,y-2),(x-1,y+1)]
    -- Filter out the values that arent within the 8x8 grid
    guard (x' `elem` [1..8] && y' `elem` [1..8])
    -- Return remaining values
    Prelude.return (x',y')

threeMoves :: KnightPos -> [KnightPos]
-- Takes in a position then finds every position a knight can get to within 3 moves from that initial position
threeMoves start = do
    move1 <- knightMovement start
    move2 <- knightMovement move1
    knightMovement move2

withinReach :: KnightPos -> KnightPos -> Bool 
-- Will see if the given position pos1 is in the set of all posisition that pos0 can reach in 3 moves
withinReach pos0 pos1 = pos1 `elem` threeMoves pos0


main = do
    -- print $ (\x y -> (x+1) * y) <$> Just pi Prelude.<*> Just pi 
    print $ Just' "Mich" Main.>>= (\x -> Main.return $ x ++ " Ochieng Monad")
    print $ slipped $ Just (0,0) Prelude.>>= landRight 2 Prelude.>>= landLeft 2 Prelude.>>= landRight 1
    print tightRope
    print $ slipped tightRope
    