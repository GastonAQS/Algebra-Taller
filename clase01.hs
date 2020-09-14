{-# LANGUAGE FlexibleContexts #-}

doble x = 2 * x

suma x y = x + y

normalVectorial x1 x2 = sqrt (x1^2) (x2^2)

funcionConstante8 _ = 8

absoluto :: Int -> Int
absoluto x | x < 0 = -x
absoluto x = x

maximoAbsoluto :: Int -> Int -> Int
maximoAbsoluto a b = max (absoluto a) (absoluto b)

maximo3 :: Int -> Int -> Int -> Int
maximo3 a b c = max (max a b) c

algunoes0 :: Int -> Int -> Bool
algunoes0 _ 0 = True
algunoes0 0 _ = True
algunoes0 _ _ = False

-- algunoes0 a b | (a == 0) || (b == 0) = True
--               | otherwise = False

ambosSon0 :: Int -> Int -> Bool
ambosSon0 0 0 = True
ambosSon0 _ _ = False

-- ambosSon0 a b | (a == 0) && (b == 0) = True
--               | otherwise = False

esMultiploDe :: Int -> Int -> Bool
esMultiploDe a b = mod a b == 0

digitoUnidades :: Int -> Int
digitoUnidades a = mod a 10

digitoDecenas :: Int -> Int
digitoDecenas a = div a 10


