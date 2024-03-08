{- EJERCICIO 2 -}
import Data.Maybe ()
import Data.Char ()
import GHC.Real ()
import Distribution.Simple.Utils (xargs)

{- EJERCICIO 1 -}
-- null :: Foldable t => t a -> Bool        (se fija si esta vacío)     (Foldable = contenedor)
-- head :: GHC.Stack.Types.HasCallStack => [a] -> a     (1er elemento de la lista)
-- tail :: GHC.Stack.Types.HasCallStack => [a] -> [a]   (devuelve lista menos el 1er elemento)
-- init :: GHC.Stack.Types.HasCallStack => [a] -> [a]   (devuelve todo menos el ultimo)
-- last :: GHC.Stack.Types.HasCallStack => [a] -> a     (devuelve el ultimo)
-- take :: Int -> [a] -> [a]    (slice la cantidad de elementos)
-- drop :: Int -> [a] -> [a]    (al reves, saca las primeros x elementos)
-- (++) :: [a] -> [a] -> [a]    (concatena listas)
-- concat :: Foldable t => t [a] -> [a]     (concatena listas dadas la coleccion de listas)
-- (!!) :: GHC.Stack.Types.HasCallStack => [a] -> Int -> a      (devuelve el elemento en el indice indicado)
-- elem :: (Foldable t, Eq a) => a -> t a -> Bool (chequea si un elemento esta en el foldable) //// elem 3 [1,2,4,3] => True //// elem 3 [1,2,4] => False

{- EJERCICIO 2 -}
valorAbsoluto :: Float -> Float
valorAbsoluto x |x < 0 = -x --valores negativos entre parentesis
                 |otherwise = x
-- divisble x 4 pero no x 100, o 400 
 
bisiesto :: Int -> Bool
bisiesto x | (mod x 4 == 0 && mod x 100 /= 0) || mod x 400 == 0  = True
           |otherwise = False
        
factorial :: Int -> Int
factorial x | x <= 1 = 1 -- x == 1 || x == 0 = 1 (o menor a 1)
            | otherwise = x * factorial(x-1) --ojo con no saltearse casos
factorial12 :: Int -> Int -- Contemplando todos los casos
factorial12 x | x == 1 = 1 -- x == 1 || x == 0 = 1
              | x <= 0 = error "factorial no posible"
              | x > 1 = x * factorial12(x-1)

-- test xs = [y| y <- xs, 'predicado']
divisores :: Int -> [Int]
divisores x = [y| y<-[1..x], mod x y == 0]

esNumPrimo :: Int -> Bool
esNumPrimo x | length (divisores x) == 2 = True
             | otherwise = False

-- esNumPrimo x length (divisores x) == 2

cantDivisoresPrimos :: Int -> Int
cantDivisoresPrimos x = length [y | y <-  divisores x, esNumPrimo y]

{- EJERCICIO 3 -}
inverso :: Float -> Maybe Float
inverso 0 = Nothing
inverso x = Just (1/x)

aEntero :: Either Int Bool -> Int
aEntero (Left x) = x
aEntero (Right False) = 0
aEntero (Right True) = 1

{- EJERCICIO 4 -}
limpiar :: String -> String -> String
limpiar string1 string2 = [y| y <- string2, not (elem y string1)] --listas xs ys (la s sugiere lista)

difPromedio :: [Float] -> [Float] -- PREGUNTAR EL FROM INTEGRAL
difPromedio lista_num = [y - (sum lista_num/ fromIntegral (length lista_num)) | y <- lista_num]

todosIguales :: [Int]->Bool 
todosIguales lista | length lista <= 1 = True
                   | head lista == lista !! 1 = todosIguales (tail lista)
                   | otherwise = False

{- EJERCICIO 5 -}
data AB a = Nil  | Bin (AB a) a (AB a) deriving (Eq, Show) -- Arbol Binario --> Nil :: AB a

nullTree :: AB a -> Bool --indica si el arbol es vacio
nullTree Nil = True
nullTree x = False

negTree :: AB Bool -> AB Bool
negTree Nil = Nil
negTree (Bin ramaIzq True ramaD) =  Bin (negTree ramaIzq) False (negTree ramaD)
negTree (Bin ramaIzq False ramaD) =  Bin (negTree ramaIzq) True (negTree ramaD)

prodTree :: AB Integer -> Integer -- que calcula el producto de todos los nodos del  ́arbol.
prodTree Nil = 1
prodTree (Bin ramaIzq num ramaD) = num * prodTree ramaIzq * prodTree ramaD

arbolBinario1 = Bin (Bin Nil 2 Nil) 1 (Bin (Bin Nil 4 Nil) 3 Nil)
arbolBinario2 = Nil
arbolBinario3 = Bin (Bin Nil True Nil) False (Bin (Bin Nil True Nil) False Nil)

{- EJERCICIO 6 -} -- HACER!!!!!!!!!!!!!!!!!!!!
max2 :: Ord a => (a, a) -> a
max2(x,y) |x>=y=x
        | otherwise = y

normaVectorial :: Floating a => (a, a) -> a
normaVectorial (x,y) = sqrt (x^2+y^2)

subtract1 :: Integer -> Integer -> Integer
subtract1 = flip (-)

restarUno :: Integer -> Integer
restarUno = subtract1 1

evaluarEnCero :: (Integer -> t) -> t
evaluarEnCero = \f->f 0

dosVeces :: (a -> a) -> a -> a
dosVeces = \f->f.f

flipAll :: [a -> b -> c] -> [b -> a -> c]
flipAll = map flip

