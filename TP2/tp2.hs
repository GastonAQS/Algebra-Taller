-- Auxiliares
seRepiteNenL :: Int -> [Int] -> Bool
seRepiteNenL _ [] = False
seRepiteNenL n (x:xs) | n == x = True
                    | otherwise = seRepiteNenL n xs

hayRepetidos :: [Int] -> Bool
hayRepetidos [] = False
hayRepetidos (x:xs) = seRepiteNenL x xs || hayRepetidos xs

aux :: [Int] -> Bool
aux [] = True
aux [_] = True
aux (x:y:xs) = sonClub x y && aux xs

eliminarRepetidosAlFinal :: [Int] -> [Int]
eliminarRepetidosAlFinal [] = []
eliminarRepetidosAlFinal (x:xs) | seRepiteNenL x xs = x:eliminarRepetidosAlFinal (quitarTodas x xs)
                                | otherwise = x:eliminarRepetidosAlFinal xs

quitarTodas :: Int -> [Int] -> [Int]
quitarTodas _ [] = []
quitarTodas n (x:xs) | n == x = quitarTodas n xs
                    | otherwise = x:quitarTodas n xs




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
sonSociables l = not (hayRepetidos l) && sonClub (last l) (head l) && aux l


sonClub :: Int -> Int -> Bool
sonClub n k = sumaDeDivisoresPropios n == k

minimosDeKClubesMenoresQue :: Int -> Int -> [Int]
minimosDeKClubesMenoresQue k c = eliminarRepetidosAlFinal (minimosDeKClubesMenoresQueDesde 1 k c)

minimosDeKClubesMenoresQueDesde :: Int -> Int -> Int -> [Int]
minimosDeKClubesMenoresQueDesde n k c | n == c = []
minimosDeKClubesMenoresQueDesde n k c | sonSociables (listaAlicuotaDeNDeLargo k n) = minimum (listaAlicuotaDeNDeLargo k n):minimosDeKClubesMenoresQueDesde (n+1) k c
                               | otherwise = minimosDeKClubesMenoresQueDesde (n+1) k c

listaDeNClubesConNrosMenoresQue :: Int -> Int -> [[Int]]
listaDeNClubesConNrosMenoresQue n k =  listaDeNClubesConNrosMenoresQueDesde 1 n k

listaDeNClubesConNrosMenoresQueDesde :: Int -> Int -> Int -> [[Int]]
listaDeNClubesConNrosMenoresQueDesde t n k | t == k = []
listaDeNClubesConNrosMenoresQueDesde t n k | sonSociables (listaAlicuotaDeNDeLargo n t) = listaAlicuotaDeNDeLargo n t:listaDeNClubesConNrosMenoresQueDesde (t+1) n k
                                           | otherwise = listaDeNClubesConNrosMenoresQueDesde (t+1) n k

