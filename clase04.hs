sumatoria1 :: Int -> Int
sumatoria1 0 = 1
sumatoria1 n = 2^n + sumatoria1 (n-1)

sumatoria2 :: (Fractional a, Eq a, Integral b, Eq b) => a -> b -> a
sumatoria2 1 _ = -1
sumatoria2 q 1 = (1-q^1)/(1-q)
sumatoria2 q n = (1-q^n)/(1-q) + sumatoria2 q (n-1)