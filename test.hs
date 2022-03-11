import Prelude hiding (Applicative)

{-
    rev :: [a] -> [a]
    rev = foldl (\acc x -> x : acc) []

    -- Regular
    size :: Tree a -> Int
    size EmptyTree = 0
    size (Node _ ell arr) = 1 + size ell + size arr
    -- _ (Whatever the Nodes value is)
    -- Ell (Left Node) arr (RightSide Node)
    -- 1 + recursiveCall(left) + recursiveCall(RightSide)

    -- Tail recursive
    size :: Tree a -> Int
    size tree = size3 0 [] tree where
        size3 acc problems EmptyTree = go acc problems
        size3 acc problems (Node _ ell arr) =
            size3 (1+acc) (arr:problems) ell
        go acc []      = acc
        go acc (p1:ps) = size3 acc ps p1

    -- Continuation passing style
    size :: Tree a -> Int
    size tree = size2 id tree where
        size2 :: (Int -> Int) -> Tree a -> Int
        -- Takes some function (Int to Int) and a tree then gives an Int
        size2 f EmptyTree = f 0
        size2 f (Node _ ell arr) =
            size2 (leftCont f arr) ell
            -- ell is the left tree
            -- 
        leftCont :: (Int -> Int) ->  Tree a -> (Int -> Int)
        -- Takes a function, a tree and returns another function
        leftCont f arr ellSize =
            size2 (RightSideCont f ellSize) arr
        RightSideCont :: (Int -> Int) -> Int -> (Int -> Int)
        RightSideCont f ellSize arrSize =
            f(1 + ellSize + arrSize)
-}

-- mirror should be a Tree a -> Tree a function

-- here's a continuation representing data type
-- data Tree a     = EmptyTree | Node a (Tree a) (Tree a) deriving (Show)
-- data Side = LeftSide | RightSide
-- data TreeCont a
--    = IdCont
--    | Cont Side a (Tree a) (TreeCont a)


-- mirror :: Tree a -> Tree a
-- mirror = mirrorX IdCont -- This will just give back EmptyTree?
-- mirrorX will return a function that gives a Tree a
-- Then mirror will give us whatever Tree a we got from mirrorX

-- mirrorX :: TreeCont a -> (Tree a -> Tree a)
-- mirrorX k EmptyTree = interpret k EmptyTree
{-
    k: IdCont
    gives result of interpret(IdCont,EmptyTree) back to mirror
-}
-- mirrorX k (Node a ell arr) = mirrorX (Cont LeftSide a arr) ell
{-
    Node _ _ _ is a function i.e the (Tree a -> Tree a) function
    k is still some TreeCont
-}

-- interpret :: TreeCont a -> Tree a -> Tree a
-- interpret IdCont t = t
-- interpret (Cont LeftSide a arr) t = mirrorX (Cont RightSide a t) ell
-- interpret (Cont RightSide a mL) t = interpret k (Node a t mL)

