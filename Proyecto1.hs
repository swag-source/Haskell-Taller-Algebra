{--Ejercicio 1--}
satisfaceCollatz :: Integer-> Integer -> Bool
satisfaceCollatz n m | n == 1 && m >= 1 = True
                     | n >= 2 && m <= 1 = False
                     | esPar n = satisfaceCollatz (div n 2) (m - 1)
                     | not (esPar n) = satisfaceCollatz (3*n + 1) (m - 1)

                    
{--Ejercicio 2--}
satisfaceCollatzHasta :: Integer -> Integer -> Bool
satisfaceCollatzHasta n m | n == 1 = True
                          | satisfaceCollatz n m == False = False
                          | satisfaceCollatz n m = satisfaceCollatzHasta (n - 1) m    

{--Ejercicio 3--}
cantidadTerminosPares :: Integer -> Integer
cantidadTerminosPares n | n == 1 = 0
                        | esPar n = 1 + cantidadTerminosPares (div n 2)
                        | not (esPar n) = cantidadTerminosPares (3*n+1)    
                        
{--Ejercicio 4--}                 
{-- Esta funcion la utilizamos como auxiliar para el ejercicio y luego nos dimos cuenta que era funcional para ele ejercicio 4 --}
largoSecuencia :: Integer -> Integer
largoSecuencia n | n == 1 = 0
                 | esPar n = 1 + largoSecuencia (div n 2)
                 | not (esPar n) = 1 + largoSecuencia (3*n+1)
                
                
{--Ejercicio 5--}
secuenciaMasLargaHasta :: Integer -> Integer
secuenciaMasLargaHasta n | n == 1 = 0
                         | satisfaceCollatzHasta (n-1) (largoSecuencia n) = n
                         | otherwise = secuenciaMasLargaHasta (n-1)

                 
{-- Funciones Auxiliares --}
esPar :: Integer -> Bool
esPar n | mod n 2 == 0 = True
        | otherwise = False




