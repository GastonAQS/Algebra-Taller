import Clase06(eliminarRepetidosAlFinal, concatenar)

type Set a = [a]

vacio :: Set Int
vacio = []

agregar :: Int -> Set Int -> Set Int
agregar x c | pertenece x c = c
agregar x c = x:c

pertenece :: Int -> Set Int -> Bool
pertenece _ [] = False
pertenece x (y:ys) = x == y || pertenece x ys

incluido :: Set Int -> Set Int -> Bool
incluido [] _ = True
incluido (x:xs) c = pertenece x c && incluido xs c

iguales :: Set Int -> Set Int -> Bool
iguales c1 c2 = incluido c1 c2 && incluido c2 c1

interseccion :: Set Int -> Set Int -> Set Int
interseccion [] _ = []
interseccion _ [] = []
interseccion (x:xs) c2 | pertenece x c2 = x:interseccion xs c2
                | otherwise = interseccion xs c2

union :: Set Int -> Set Int -> Set Int
union [] c = c
union c [] = c
union c1 c2 = eliminarRepetidosAlFinal (concatenar c1 c2)

diferencia :: Set Int -> Set Int -> Set Int
diferencia [] _ = []
diferencia (x:xs) (y:ys) | pertenece x (y:ys) = diferencia xs (y:ys)
                         | otherwise = x:diferencia xs (y:ys)

diferenciaSimetrica :: Set Int -> Set Int -> Set Int
diferenciaSimetrica c1 c2 = union (diferencia c1 c2) (diferencia c2 c1)

agregarC :: Set Int -> Set (Set Int) -> Set (Set Int)
agregarC xs xss | perteneceC xs xss = xss
                | otherwise = xs:xss

perteneceC :: Set Int -> Set (Set Int) -> Bool
perteneceC _ [] = False
perteneceC xs (ys:yss) = iguales xs ys || perteneceC xs yss



agregarATodos :: Int -> Set (Set Int) -> Set (Set Int)
agregarATodos _ [] = []
agregarATodos x (c:cs) = agregarC (agregar x c) (agregarATodos x cs)

-- partes :: Set Int -> Set (Set Int)
-- partes [] = [[]]
-- partes (x:xs) = union (partes xs) (agregarATodos x (partes xs))






