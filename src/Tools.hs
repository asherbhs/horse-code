module Tools (padRight) where

padRight :: Integral a => a -> b -> [b] -> [b]
padRight n p xs
    | n <= 0 = xs
    | otherwise = xs ++ replicate n p