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