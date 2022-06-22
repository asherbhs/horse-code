module Tools (padRight) where

padRight ::  Int -> a -> [a] -> [a]
padRight n p xs
    | n <= 0    = xs
    | otherwise = xs ++ replicate (n - length xs) p