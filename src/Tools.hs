module Tools 
    ( padRight
    , iff
    , (?)
    ) where

iff :: Bool -> a -> a -> a
iff p x y = if p then x else y

infixr 1 ?
(?) :: Bool -> a -> a -> a
(?) = iff

padRight ::  Int -> a -> [a] -> [a]
padRight n p xs
    | n <= 0    = xs
    | otherwise = xs ++ replicate (n - length xs) p