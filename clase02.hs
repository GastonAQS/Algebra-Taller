prodInt :: Num t => (t, t) -> (t, t) -> t
prodInt (x1, y1) (x2, y2) = (x1 * y1) + (x2 * y2)

todoMenor :: (Num t, Ord t) => (t, t) -> (t, t) -> Bool
todoMenor (x1, y1) (x2, y2) = (x1 < x2) && (y1 < y2)

distanciaPuntos :: (Floating t) => (t, t) -> (t, t) -> t
distanciaPuntos (x1, y1) (x2, y2) = sqrt ((x2 - x1)**2 + (y2 -y1)**2)

sumaTerna :: (Int, Int, Int) -> Int
sumaTerna (a, b, c) = a + b + c

crearPar :: a -> b -> (a, b)
crearPar a b = (a,b)

invertir :: (a, b) -> (b, a)
invertir (a, b) = (b, a)
