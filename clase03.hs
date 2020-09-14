import Clase01(digitoUnidades, digitoDecenas)
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib(n - 1) + fib (n - 2)

-- parteEntera :: Float -> Int

esMultiploDe3 :: Int -> Bool
esMultiploDe3 0 = True
esMultiploDe3 n | n < 0 = False
esMultiploDe3 n = esMultiploDe3 (n - 3)

sumaImpares :: Int -> Int
sumaImpares 1 = 1
sumaImpares n = (2*n - 1) + sumaImpares (n-1)

medioFact :: Int -> Int
medioFact 0 = 1
medioFact 1 = 1
medioFact n = n*(medioFact (n-2))

cantidadDeDigitos :: Int -> Int
cantidadDeDigitos 0 = 1
cantidadDeDigitos n | n <= 9 = 1
cantidadDeDigitos n = 1 + cantidadDeDigitos (div n 10)

sumaDigitos :: Int -> Int
sumaDigitos n | cantidadDeDigitos n == 1 = n
sumaDigitos n = digitoDecenas n (10^((cantidadDeDigitos n) - 1)) + sumaDigitos (digitoUnidades n)

digitosIguales :: Int -> Bool
digitosIguales n | cantidadDeDigitos n == 1 = True
digitosIguales n = digitoDecenas n (10^((cantidadDeDigitos n) - 1)) == sumaDigitos (digitoUnidades n)
