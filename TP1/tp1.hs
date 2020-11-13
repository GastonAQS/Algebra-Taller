med :: Float -> Float -> Int -> Float
med i0 _ 0 = i0
med i0 b n = med i0 b (n-1) + (med i0 b (n-1))*b

-- A ser testeado con mas casos
mld :: Float -> Float -> Float -> Int -> Float
mld p i0 b 0 = i0
mld p i0 b n = mld p i0 b (n-1) + (mld p i0 b (n-1))*b*((p - med i0 b (n-1))/p)

sir :: (Float, Float, Float) -> Float -> Float -> Int -> (Float, Float, Float)
sir (s0, i0, r0) b g 0 = (s0, i0, r0)
sir (s0, i0, r0) b g n = 


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