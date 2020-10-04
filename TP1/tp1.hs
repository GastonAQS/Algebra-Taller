med :: Float -> Float -> Int -> Float
med i0 b n = medDesde i0 b 0 n 

medDesde :: Float -> Float -> Int -> Int -> Float
medDesde i0 b n k | n == k = 0
medDesde i0 b n k | n == 0 = i0 + i0*b + medDesde (i0 + i0*b) b (n+1) k
                  | b == 1 = i0*b + medDesde (i0 + i0) b (n+1) k
                  | otherwise = i0*b + medDesde (i0*b) b (n+1) k


mld :: Float -> Float -> Float -> Int -> Float
mld p i0 b n = 0

mldDesde :: Float -> Float -> Float -> Int -> Int -> Float
mldDesde p i0 b n k | n == k = 0
mldDesde p i0 b n k | n == 0 = 0