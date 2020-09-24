sumatoria1 :: Int -> Int
sumatoria1 0 = 1
sumatoria1 n = 2^n + sumatoria1 (n-1)

sumatoria2 :: Float -> Int -> Float
sumatoria2 q 0 = 0
sumatoria2 q n = q^n + sumatoria2 q (n-1)

sumatoria3' :: Float -> Int -> Float
sumatoria3' q n = sumatoria2 q (2*n)

sumatoria3 :: Float -> Int -> Float
sumatoria3 q 0 = 0
sumatoria3 q n = sumatoria3 q (n-1) + q^(2*n-1) + q^(2*n)

sumatoria4 :: Float -> Int -> Float
sumatoria4 q 0 = 1
sumatoria4 q n = q^(2*n-1) + q^(2*n) - q^(n-1) + sumatoria4 q (n-1)