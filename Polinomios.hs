type Polinomio = [Float]
type Monomio = (Float , Int)

-- Ejercicio 1 --
crearPolinomio :: [Float] -> Polinomio
crearPolinomio p | head p == 0 = crearPolinomio (tail p)
                 | otherwise = p

-- Ejercicio 2 --
grado :: Polinomio -> Int 
grado p | tail p == [] = 0
        | otherwise = 1 + grado (tail p)

-- Ejercicio 3 --
evaluar :: Polinomio -> Float -> Float
evaluar p a | grado p == 0 = head p 
            | otherwise = (head p) * (a ^ grado p) + evaluar (tail p) a        

-- Ejercicio 4 --
productoPorMonomio :: Monomio -> Polinomio -> Polinomio
productoPorMonomio (a,n) [] = []
productoPorMonomio (a,n) p | grado p == 0 = (a*(head p)  : agregarCeros n)
                           | otherwise = (a*(head p) : productoPorMonomio (a,n) (tail p))

-- Ejercicio 5 --
producto :: Polinomio -> Polinomio -> Polinomio
producto p1 p2 = sumatoriaDePolinomios (distribuirPolinomio p1 p2)

-- Ejercicio 6 --
evaluacionMultiple :: [Float] -> Polinomio -> Polinomio -> [Float]
evaluacionMultiple ls p q | ls == [] = []
                          | (posicion ls) `mod` 2 == 0 = (evaluar p (head ls) : evaluacionMultiple (tail ls) p q)
                          | otherwise = (evaluar q (head ls) : evaluacionMultiple (tail ls) p q)

-- Funciones auxiliares --

-- Se aplica en Ejercicio 4 --
agregarCeros:: Int -> [Float]
agregarCeros 0 = []
agregarCeros n = (0 : agregarCeros (n-1))

-- Se aplican en Ejercicio 5 --
sumaDeDosPolinomios :: Polinomio -> Polinomio -> Polinomio
sumaDeDosPolinomios p1 [] = p1
sumaDeDosPolinomios [] p2 = p2
sumaDeDosPolinomios p1 p2 | (grado p1) > (grado p2) = ((head p1) : (sumaDeDosPolinomios (tail p1) p2))
                          | (grado p2) > (grado p1) = ((head p2) : (sumaDeDosPolinomios p1 (tail p2)))
                          | (grado p1) == (grado p2) = ((head p1 + head p2) : (sumaDeDosPolinomios (tail p1) (tail p2)))

sumatoriaDePolinomios :: [Polinomio] -> Polinomio
sumatoriaDePolinomios ps | ps == [] = []
                         | otherwise = sumaDeDosPolinomios (head ps) (sumatoriaDePolinomios (tail ps))

distribuirPolinomio :: Polinomio -> Polinomio -> [Polinomio]
distribuirPolinomio p1 p2 | p1 == [] = []
                          | p2 == [] = []
                          | otherwise = ((productoPorMonomio (head p1 , grado p1) p2) : (distribuirPolinomio (tail p1) p2))

-- Se aplican en Ejercicio 6 --
posicion :: [Float] -> Int
posicion [_] = 0
posicion (x:xs) = 1 + posicion xs

