module Tools (padRight) where

padRight ::  Int -> b -> [b] -> [b]
padRight n p xs
    | n <= 0 = xs
    | otherwise = xs ++ replicate (n - length xs) p