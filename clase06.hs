sumatoria :: [Int] -> Int
sumatoria [] = 0
sumatoria l = head l + sumatoria (tail l)

longitud :: [Int] -> Int
longitud [] = 0
longitud l = 1 + longitud (tail l)

pertenece :: Int -> [Int] -> Bool
pertenece _ [] = False
pertenece x l = x == head l || pertenece x (tail l)

primerMultiploDe45345 :: [Int] -> Int
primerMultiploDe45345 l | rem (head l) 45345 == 0 = head l
                        | otherwise = primerMultiploDe45345 (tail l)

pertenecePM :: Int -> [Int] -> Bool
pertenecePM _ [] = False
pertenecePM t (x:xs) = t == x || pertenecePM t xs

productoria :: [Int] -> Int
productoria (x:xs) = x * productoria xs

sumarN :: Int -> [Int] -> [Int]
sumarN _ [] = []
sumarN n (x:xs) = (n + x):(sumarN n xs)

sumarElPrimero :: [Int] -> [Int]
sumarElPrimero (x:xs) = sumarN x (x:xs)

sumarElUltimo :: [Int] -> [Int]
sumarElUltimo (x:xs) = sumarN (last xs) (x:xs)

quitar :: Int -> [Int] -> [Int]
quitar n (x:xs) | n == x = xs
                | otherwise = x:(quitar n xs)

quitarTodas :: Int -> [Int] -> [Int]
quitarTodas _ [] = []
quitarTodas n (x:xs) | n == x = quitarTodas n xs
                     | otherwise = x:(quitarTodas n xs)

seRepiteNenL :: Int -> [Int] -> Bool
seRepiteNenL _ [] = False
seRepiteNenL n (x:xs) | n == x = True
                     | otherwise = seRepiteNenL n xs

hayRepetidos :: [Int] -> Bool
hayRepetidos [] = False
hayRepetidos (x:xs) = seRepiteNenL x xs || hayRepetidos xs

eliminarRepetidosAlFinal :: [Int] -> [Int]
eliminarRepetidosAlFinal [] = []
eliminarRepetidosAlFinal (x:xs) | seRepiteNenL x xs = x:(eliminarRepetidosAlFinal (quitarTodas x xs))
                                | otherwise = x:eliminarRepetidosAlFinal xs

eliminarRepetidosAlInicio :: [Int] -> [Int]
eliminarRepetidosAlInicio (x:xs) = reverse (eliminarRepetidosAlFinal (reverse (x:xs)))

maximo :: [Int] -> Int
maximo [] = -1
maximo (x:xs) = max x (maximo xs)

ordenar :: [Int] -> [Int]
ordenar [] = []
ordenar [x] = [x]
ordenar (x:y:xs) | y > x = x:ordenar (y:xs)
                 | otherwise = y:ordenar (x:xs)

ordenarCompleto :: [Int] -> [Int]
ordenarCompleto l = iterate ordenar l !! length l

reverso :: [Int] -> [Int]
reverso [] = []
reverso (x:xs) = reverso xs ++ [x]

concatenar :: [Int] -> [Int] -> [Int]
concatenar (x:xs) (y:ys) = (x:xs) ++ (y:ys)

zipi :: [a] -> [b] -> [(a,b)]
zipi [] [] = []
zipi [] _ = []
zipi _ [] = []
zipi (x:xs) (y:ys) = [(x,y)] ++ (zipi xs ys)