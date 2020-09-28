sumatoria :: Int -> Int
sumatoria 0 = 0
sumatoria n = n + sumatoria (n-1)

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

factorial :: Int -> Int
factorial 1 = 1
factorial n = n*(factorial (n-1))

eAprox :: Int -> Float
eAprox 0 = 1
eAprox n = (1/(fromIntegral (factorial n))) + eAprox (n-1)

e :: Float
e = eAprox 10

f :: Int -> Int -> Int
f n 0 = 0
f n m = (f (n-1) m) + round (sumatoria2 (fromIntegral n) m)

sumaPotencias :: Float -> Int -> Int -> Float
sumaPotencias q n 0 = 0
sumaPotencias q n m = (q^m*(sumatoria2 q n)) + sumaPotencias q n (m-1)

sumaRacionales :: Int -> Int -> Float
sumaRacionales n 0 = 0
sumaRacionales n m = (fromIntegral (sumatoria n))/(fromIntegral m) + sumaRacionales n (m-1)

g1 :: Int -> Int -> Int
g1 i n | n < i = 0
g1 i n = n^n + g1 i (n-1)

g2 :: Int -> Int
g2 0 = 0
g2 n = (sumatoria n)^n + g2 (n-1)

g3 :: Int -> Int
g3 0 = 1
g3 n | rem n 2 == 0 = 2^n + g3 (n-1)
g3 n = g3 (n-1)
