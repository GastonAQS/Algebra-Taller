med :: Float -> Float -> Int -> Float
med i0 b n = medDesde i0 b 0 n 

medDesde :: Float -> Float -> Int -> Int -> Float
medDesde i0 b n k | n == k = 0
medDesde i0 b n k | n == 0 = i0 + i0*b + medDesde (i0 + i0*b) b (n+1) k
                  | b == 1 = i0*b + medDesde (i0 + i0) b (n+1) k
                  | otherwise = i0*b + medDesde (i0*b) b (n+1) k


mld :: Float -> Float -> Float -> Int -> Float
mld p i0 b n = mldDesde p i0 b 0 n

mldDesde :: Float -> Float -> Float -> Int -> Int -> Float
mldDesde p i0 b n k | n == k = 0
mldDesde p i0 b n k | n == 0 = i0 + i0*b*((p - i0)/p) + mldDesde p (i0 + i0*b*((p - i0)/p)) b (n+1) k
                    | otherwise = i0*b*((p - i0)/p) + mldDesde p (i0 + i0*b*((p - i0)/p)) b (n+1) k

sir :: (Float, Float, Float) -> Float -> Float -> Int -> (Float, Float, Float)
sir (s0, i0, r0) b g n = sirDesde (s0, i0, r0) b g 0 n

operaSir :: Float -> Float -> Float -> Float -> Float -> (Float, Float, Float)
operaSir s0 i0 r0 b g = (s0 - b*i0*s0, i0 + b*i0*s0 - g*i0, r0 + g*i0)

sirDesde :: (Float, Float, Float) -> Float -> Float -> Int -> Int -> (Float, Float, Float)
sirDesde (s0, i0, r0) b g n k | n == k = (s0, i0, r0)
sirDesde (s0, i0, r0) b g n k | n < k = sirDesde (operaSir s0 i0 r0 b g) b g (n+1) k
                             
maxsir :: (Float, Float, Float) -> Float -> Float -> Int -> Float
maxsir (s0, i0, r0) b g n = maxsirDesde (s0,i0,r0) b g 0 n

maxsirDesde :: (Float, Float, Float) -> Float -> Float -> Int -> Int -> Float
maxsirDesde (s0,i0,r0) b g n k | n == k = i0
maxsirDesde (s0,i0,r0) b g n k | n < k = max i0 (maxsirDesde (operaSir s0 i0 r0 b g) b g (n+1) k)