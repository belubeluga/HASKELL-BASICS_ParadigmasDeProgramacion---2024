{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use splitAt" #-}

-- CURRIFICACION Y TIPOS EN HASKELL
{- EJERCICIO 6 -} -- preguntar!!!!!!!!!!!!!!!!!!

max2 :: Ord a => (a, a) -> a
max2(x,y) |x>=y=x
        | otherwise = y -- currificada

normaVectorial :: Floating a => (a, a) -> a
normaVectorial (x,y) = sqrt (x^2+y^2) -- currificada

subtract1 :: Integer -> Integer -> Integer
subtract1 = flip (-) -- currificada

restarUno :: Integer -> Integer
restarUno = subtract1 1 -- currificada

evaluarEnCero :: (Integer -> t) -> t
evaluarEnCero f = f 0 -- currificada

dosVeces :: (a -> a) -> a -> a
dosVeces f = f.f -- currificada

flipAll :: [a -> b -> c] -> [b -> a -> c]
flipAll = map flip -- NO currificada

curryflipAll :: [a -> b -> c] -> [b -> a -> c]
curryflipAll = map flip' -- PREGUNTAR
    where flip' = flip


{- EJERCICIO 7 -}
-- definir la funcion curry, dada una funcion de dos argumentos devuelve su equivalente currificada
curry2 :: ((a, b) -> t) -> a -> b -> t --PREGUNTAR
curry2 funcion x y = funcion (x, y)

unCurry :: (t1 -> t2 -> t3) -> (t1, t2) -> t3
unCurry funcion (x,y) = funcion x y

suma :: (Int, Int) -> Int
suma (x, y) = x + y
sumaCurry = curry2 suma
suma2 = unCurry sumaCurry

{- EJERCICIO 8 -}
-- LISTAS POR COMPRENSIÓN
listas = [ x | x <- [1..2], y <- [x..3], even (x+y) ] -- devuelve [1,1,2]

{- EJERCICIO 9 -}
{-Una tripla pitag ́orica es una tripla (a,b,c) de enteros positivos tal que a2 + b2 = c2.
La siguiente expresio ́n intenta ser una definicion de una lista (infinita) de triplas pitagorica.
Explicar por que esta definicio ́n no es u ́til. Dar una definici ́on mejor-}
pitagoricas :: [(Integer,Integer,Integer)]
pitagoricas = [(a,b,c) | c <- [1..], a <- [1..c], b <-[1..c], a^2 + b^2 == c^2] -- OJO CON EL ORDEN DENTRO DE LOS CLOSURES
-- cambio de orden para que vaya devolviendo valores (sino no devolvia)


{- EJERCICIO 10 -}
-- generar una lista de los primeros mil num primos. evaluacion lazy
divisores :: Int -> [Int]
divisores x = [y| y<-[1..x], mod x y == 0]

esNumPrimo :: Int -> Bool
esNumPrimo x | length (divisores x) == 2 = True
             | otherwise = False

milPrimos :: [Int]
milPrimos = [x| x <-[0..], (length (show x) < 1000) && esNumPrimo x]


{- EJERCICIO 11 -}
{-Usando listas por comprension, escribir la funcion partir::[a]->[([a],[a])] que, dada una lista xs, 
devuelve todas las maneras posibles de partirla en dos sublistas xs1 y xs2 tales que xs1++xs2 == xs.
Ejemplo: partir [1,2,3] devuelve [([],[1,2,3]),([1],[2,3]),([1,2],[3]),([1,2,3],[])]  (conjunto de partes) -}
partir :: [num] -> [([num],[num])]
partir xs = [splitAt x xs| x <- [0..length xs]]


{- EJERCICIO 12 -}
-- dado n num natural -> devuelve todas las listas de enteros positivos cuya suma sea n
listasQueSuman :: Int -> [[Int]]
listasQueSuman num = [[x,y] |y <- [0..num], x <-[0..num], x + y == num] 

{- EJERCICIO 13 -}
-- definir una lista que contenga todas las listas finitas de enteros positivos (con elementos >= 1)
listasEnterosPositivos :: [Integer] -- mod x 1 == 0 por definicion
listasEnterosPositivos = [x | x<-[1..], x>=1] -- PREGUNTAR !!!


{-  EJERCICIO 14  TERMINAR!!!!!-}
-- Redefinir usando foldr las funciones sum, elem, (++), filter y map

-- sum
sumarLista :: Num a => [a] -> a
sumarLista = sum

-- elem
elem2 :: Eq a => a -> [a] -> Bool
elem2 elemento = foldr ((||) . (== elemento)) False

-- hacer funcion que se llame pegarAtras
        -- pegarAtras 1 [2,3,4] == [2,3,4,1]

pegarAtras :: a -> [a] -> [a]
pegarAtras elemento lista = reverse (elemento : reverse lista) -- usando constructor de lista

-- redefinir ++ usando foldr, pegarAtras y composicion
concat2 a  = foldr pegarAtras a . reverse

-- 2)
sumaAlt :: (Foldable f, Num a) => f a -> a
sumaAlt = foldr (-) 0

sumaAltReves = sumaAlt. reverse

{- EJERCICIO 15 -}
-- 1)


-- 2) funcion prefijos (usar foldl)
{- [[5,1,2]] -> [[], [5], [1], [2], [5,1], ] -}

func :: [[a]] -> [[a]]
func xss = tail xss ++ xss

--prefijo :: [[a]] -> [[a]]
--prefijo = foldl func [[]]. reverse 

-- 3)