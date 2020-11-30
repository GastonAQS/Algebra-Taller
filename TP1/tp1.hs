med :: Float -> Float -> Int -> Float
med i0 _ 0 = i0
med i0 b n = infectadosAyer + infectadosAyer*b
    where infectadosAyer = med i0 b (n-1)

mld :: Float -> Float -> Float -> Int -> Float
mld _ i0 _ 0 = i0
mld p i0 b n = infectadosAyer + infectadosAyer*b*(sanosAyer/p)
    where (infectadosAyer, sanosAyer) = (mld p i0 b (n-1),p - mld p i0 b (n-1))

sir :: (Float, Float, Float) -> Float -> Float -> Int -> (Float, Float, Float)
sir (s0, i0, r0) _ _ 0 = (s0, i0, r0)
sir (s0, i0, r0) b g n = (sanosAyer - b*infAyer*sanosAyer, infAyer + b*infAyer*sanosAyer - g*infAyer, recuAyer + g*infAyer)
    where (sanosAyer, infAyer, recuAyer) = sir (s0, i0, r0) b g (n-1)

devuelveNValor :: (Float, Float, Float) -> Float -> Float
devuelveNValor (s0,_,_) 1 = s0
devuelveNValor (_,i0,_) 2 = i0
devuelveNValor (_,_,r0) 3 = r0
                             
maxsir :: (Float, Float, Float) -> Float -> Float -> Int -> Float
maxsir (s0, i0, r0) b g n = maxsirDesde (s0,i0,r0) b g 1 n

maxsirDesde :: (Float, Float, Float) -> Float -> Float -> Int -> Int -> Float
maxsirDesde (s0,i0,r0) b g n k | n == k = devuelveNValor (sir (s0,i0,r0) b g (n-1)) 2
maxsirDesde (s0,i0,r0) b g n k | n < k = max i0 (maxsirDesde (sir (s0,i0,r0) b g n) b g (n+1) k)

mejora :: (Float, Float, Float) -> Float -> Float -> Int -> Bool
mejora (_,i0,r0) _ _ 0 = r0 > i0
mejora (s0,i0,r0) b g n | recuperadosDiaN > infectadosDiaN = True
                        | otherwise = mejora (s0,i0,r0) b g (n-1)
    where (recuperadosDiaN, infectadosDiaN) = (devuelveNValor (sir (s0,i0,r0) b g n) 3, devuelveNValor (sir (s0,i0,r0) b g n) 2)