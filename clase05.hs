import Clase03(fib)

prod :: Int -> Int -> Int
prod d h | d == h = d
prod d h = h*(prod d (h-1))

factorial :: Int -> Int
factorial 1 = 1
factorial n = prod 1 n

prod' :: Int -> Int -> Int
prod' d h | d == h = d
prod' d h = d*(prod' (d+1) h)

sumaDivisoresHasta :: Int -> Int -> Int
sumaDivisoresHasta n 1 = 1
sumaDivisoresHasta n k | rem n k == 0 = k + sumaDivisoresHasta n (k-1)
                       | otherwise    = sumaDivisoresHasta n (k-1)

sumaDivisores :: Int -> Int
sumaDivisores 1 = 1
sumaDivisores n = sumaDivisoresHasta n n

menorDivisorDesde :: Int -> Int -> Int
menorDivisorDesde n k | rem n k == 0 = k
                      | otherwise    = menorDivisorDesde n (k+1)

menorDivisor :: Int -> Int
menorDivisor n = menorDivisorDesde n 2

esPrimo :: Int -> Bool
esPrimo 1 = False
esPrimo n = n == menorDivisor n

nEsimoPrimo :: Int -> Int
nEsimoPrimo 1 = 2
nEsimoPrimo n = minimoPrimoDesde (1 + nEsimoPrimo (n-1))

minimoPrimoDesde :: Int -> Int
minimoPrimoDesde n | esPrimo n = n
minimoPrimoDesde n = minimoPrimoDesde (n+1)

menorFactorialDesde :: Int -> Int -> Int
menorFactorialDesde i m | factorial i >= m = factorial i
                        | otherwise        = menorFactorialDesde (i+1) m

menorFactorialHasta :: Int -> Int -> Int
menorFactorialHasta i m | factorial i >= m = factorial (i-1)
menorFactorialHasta i m = menorFactorialHasta (i+1) m

menorFactorialMayor :: Int -> Int
menorFactorialMayor n = menorFactorialDesde 1 n

mayorFactorialMenor :: Int -> Int
mayorFactorialMenor n = menorFactorialHasta 1 n

esFactDesde :: Int -> Int -> Bool
esFactDesde n k | factorial n == k = True
                | factorial n > k = False
                | otherwise = esFactDesde (n+1) k

esFact :: Int -> Bool
esFact n = esFactDesde 1 n

esFibonacciHasta :: Int -> Int -> Bool
esFibonacciHasta n k | fib n == k = True
                     | fib n > k = False
                     | otherwise = esFibonacciHasta (n+1) k

esFibonacci :: Int -> Bool
esFibonacci n = esFibonacciHasta 1 n

sumaDeNPrimos :: Int -> Int
sumaDeNPrimos 0 = 0
sumaDeNPrimos n = nEsimoPrimo n + sumaDeNPrimos (n-1)

esSumaInicialDePrimosHasta :: Int -> Int -> Bool
esSumaInicialDePrimosHasta n k | sumaDeNPrimos n == k = True 
                               | sumaDeNPrimos n > k = False
                               | otherwise = esSumaInicialDePrimosHasta (n+1) k

esSumaInicialDePrimos :: Int -> Bool
esSumaInicialDePrimos n = esSumaInicialDePrimosHasta 1 n

tomaValorMaxDesde :: Int -> Int -> Int
tomaValorMaxDesde n2 k | sumaDivisores k > n2 = sumaDivisores (k-1)
                       | otherwise = tomaValorMaxDesde n2 (k+1)

tomaValorMax :: Int -> Int -> Int
tomaValorMax n1 n2 | tomaValorMaxDesde n2 1 > n1 = tomaValorMaxDesde n2 1

