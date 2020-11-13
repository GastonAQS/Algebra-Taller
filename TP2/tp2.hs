-- Funciones Auxiliares

hayRepetidos :: [Int] -> Bool
hayRepetidos [] = False
hayRepetidos (x:xs) = elem x xs || hayRepetidos xs

esListaSociable :: [Int] -> Bool
esListaSociable [] = True
esListaSociable [_] = True
esListaSociable (x:y:xs) = coincidenDivisores x y && esListaSociable xs

eliminarRepetidosAlFinal :: [Int] -> [Int]
eliminarRepetidosAlFinal [] = []
eliminarRepetidosAlFinal (x:xs) | x `elem` xs = x:eliminarRepetidosAlFinal (quitarTodas x xs)
                                | otherwise = x:eliminarRepetidosAlFinal xs

quitarTodas :: Int -> [Int] -> [Int]
quitarTodas _ [] = []
quitarTodas n (x:xs) | n == x = quitarTodas n xs
                    | otherwise = x:quitarTodas n xs

removerListaDeLista :: [Int] -> [[Int]] -> [[Int]]
removerListaDeLista _ [] = []
removerListaDeLista l (x:xs) | l == x = xs
                             | otherwise = x:removerListaDeLista l xs

quitarElemInverso :: [[Int]] -> [[Int]]
quitarElemInverso [] = []
quitarElemInverso (x:xs) | reverse x `elem` xs = x:quitarElemInverso (removerListaDeLista (reverse x) xs)
                         | otherwise = x:quitarElemInverso xs

--

sumaDeDivisoresPropios :: Int -> Int
sumaDeDivisoresPropios = sumaDivisoresPropiosDesde 1

sumaDivisoresPropiosDesde :: Int -> Int -> Int
sumaDivisoresPropiosDesde _ 0 = 0
sumaDivisoresPropiosDesde n k | n == k = 0
sumaDivisoresPropiosDesde n k | rem k n == 0 = n + sumaDivisoresPropiosDesde (n+1) k
                              | otherwise = sumaDivisoresPropiosDesde (n+1) k

esPerfecto :: Int -> Bool
esPerfecto n = sumaDeDivisoresPropios n == n

listaAlicuotaDeNDeLargo :: Int -> Int -> [Int]
listaAlicuotaDeNDeLargo 0 _ = []
listaAlicuotaDeNDeLargo n k = k:listaAlicuotaDeNDeLargo (n-1) (sumaDeDivisoresPropios k)

sonSociables :: [Int] -> Bool
sonSociables l = not (hayRepetidos l) && coincidenDivisores (last l) (head l) && esListaSociable l

coincidenDivisores :: Int -> Int -> Bool
coincidenDivisores n k = sumaDeDivisoresPropios n == k

minimosDeKClubesMenoresQue :: Int -> Int -> [Int]
minimosDeKClubesMenoresQue k c = eliminarRepetidosAlFinal (minimosDeKClubesMenoresQueDesde 1 k c)

minimosDeKClubesMenoresQueDesde :: Int -> Int -> Int -> [Int]
minimosDeKClubesMenoresQueDesde n _ c | n == c = []
minimosDeKClubesMenoresQueDesde n k c | sonSociables (listaAlicuotaDeNDeLargo k n) && minimum (listaAlicuotaDeNDeLargo k n) <= c = minimum (listaAlicuotaDeNDeLargo k n):minimosDeKClubesMenoresQueDesde (n+1) k c
                                      | otherwise = minimosDeKClubesMenoresQueDesde (n+1) k c

listaDeNClubesConNrosMenoresQue :: Int -> Int -> [[Int]]
listaDeNClubesConNrosMenoresQue n k = quitarElemInverso (listaDeNClubesConNrosMenoresQueDesde 1 n k)

listaDeNClubesConNrosMenoresQueDesde :: Int -> Int -> Int -> [[Int]]
listaDeNClubesConNrosMenoresQueDesde t _ k | t == k = []
listaDeNClubesConNrosMenoresQueDesde t n k | sonSociables (listaAlicuotaDeNDeLargo n t) && last (listaAlicuotaDeNDeLargo n t) < k = listaAlicuotaDeNDeLargo n t:listaDeNClubesConNrosMenoresQueDesde (t+1) n k
                                           | otherwise = listaDeNClubesConNrosMenoresQueDesde (t+1) n k

