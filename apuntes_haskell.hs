------------------------------------------------------------29/02/24

{- PRACTICA DE PROGRAMACION FUNCIONAL -}

--ghci para abrir
-- ctrl L para limpiar

-- 4 + 4 (+ esta como infijo)
-- (+) 4 4 (prefijo)

--------------------------------- PREGUNTAR TIPOS
-- :t (de q tipo es el valor)
-- :t 4 ===> 4 :: Num a => a (a = num, variable de tipo)

-- :t (+) ===> (+) Num a => a -> a -> a (recibe a y a y devuelve a)

---------------------------------- TIPOS
-- listas -> un solo tipo


---------------------------------- FUNCIONES
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

{-  
        || OR       && AND
        mod RESTO   div PARTE ENTERA
        == 

-}


{- EJERCICIO 2 -}
import Data.Maybe
import Data.Char
import GHC.Real
import Distribution.Simple.Utils (xargs)

valorAbsoluto :: Float -> Float
valorAbsoluto x |x < 0 = -x --valores negativos entre parentesis
                 |otherwise = x

-- divisble x 4 o 400 pero no x 100


bisiesto :: Int -> Bool
bisiesto x | (mod x 4 == 0 || mod x 400 == 0) && (mod x 100 /= 0) = True
           |otherwise = False


-- :l funciones.hs
-- :r (recarga el primer archivo)








------------------------------------------------------------29/02/24
{- MODULARIZACION -}

-- test xs = [y| y <- xs, 'predicado']  CLOSURE              (como lista por comprension)
-- y variable
-- test [1,2,3] = []
-- devuelve los elementos de la lista que cumplen el predicado



-- data Maybe a = Nothing | Just a      (ataja el error si no devuelve nada)
-- data Either a b = Left a | Right b   (/= tipos de dato)

{-test :: Int -> Either Int Char ==> devuelve Left 3 || Right 'a'-}

f33 n | n>= 3 = 5
     | n == 2 = undefined -- tira ERROR
     | otherwise = 8


------------------------------------------------------------4/03/24
{- 

                TIPOS 
                Int = { + - * div mod }
                Float = { + - * / }
                Bool = True False { && || not }

                VARIABLES DE TIPO
                denotan familias de tipos (x el num - int o float...)

                IDENTIDAD

                -- null :: Foldable t => t a -> Bool (esto es la aridad) de null

-}

------------------------------------------------------------5/03/24
triple x = 3*x

maximo x y | x>= y = x
           | otherwise = y

distintos :: Eq a => a -> a -> Bool --(MISMO TIPO DE DATO)
distintos x y = x /= y --vale para num y caracter (cualquier tipo que sepa distinguirse = Eq)
-- Eq (todos los objetos q pueden ser distinguidos entre si)

distintos' :: (Eq a, Eq b) => a -> b -> Bool
distintos' x y = x /= x

-- IMPORTANTE LAS SANGRÍAS!!

cinco :: Int
cinco = 5
-- sqrt :: Floating a => a -> a

triple1 = triple 3 -- triple1 :: Integer
triple2 = triple 3.0 -- triple2 :: Double

{-       
        CLASES : particiones del universo

        - Integral : {Int, Integer, ...) { mod, div, ...)
        - Fractional : { Float, Double, ...) { (D,...)
        - Floating : { Float, Double, ... } ( sqrt, sin, cos, tan, ...}
        - Num : { Int, Integer, Float, Double, ... ) { (+), (*), abs, ...)
        - Ord : ( Bool, Int, Integer, Float, Double, ...) ( (<=), compare }
        - Eq: { Bool, Int, Integer, Float, Double, ... } ( (==), (/=) }      

        TIPO /= CLASE 
-}

-- cantidad de raices de x^2 + bx + c
raices :: (Num t, Ord t) => t -> t -> Int
raices b c | d > 0  = 2
           | d == 0 = 1
           | otherwise = 0
        where d = b^2 - 4 * c


f1 x y z = x ** y + z <= x + y ** z
 
f2 x y = sqrt x / sqrt y -- sqrt solo recibe float

f3 x y = div (sqrt x) (sqrt y)

f4 x y z |    x == y   = z
         | x ** y == y = z
         |  otherwise  = z
          
f5 x y z |    x == y   = z
         | x ** y == y = x
         |  otherwise  = y

cinco' :: Int
cinco' = 5

--          TUPLAS          --
-- (las unicas que admiten elementos de diferentes tipos)

suma :: (Float, Float) -> (Float, Float) -> (Float, Float)
suma (vx, vy) (wx, wy) = (vx + wx, vy + wy)

normaVectorial1 :: (Float, Float) -> Float
normaVectorial1 (x, y) = sqrt (x ^ 2 + y ^ 2)

normaVectorial2 :: Float -> Float -> Float
normaVectorial2 x y = sqrt (x ^ 2 + y ^ 2)

normalSuma :: (Float, Float) -> (Float, Float) -> Float
normalSuma v1 v2 = normaVectorial1 (suma v1 v2)

norma2Suma :: (Float, Float) -> (Float, Float) -> Float
norma2Suma v1 v2 = normaVectorial2 (fst s) (snd s)
        where s = suma v1 v2


------------------------------------------------------------7/03/24

--        TIPOS ALGEBRAICOS     --
{- armado de tipos y constructores-}

data Helado = Vasito Gusto
            | Cucurucho Gusto Gusto (Maybe Baño)
            | Capelina Gusto Gusto [Agregado]
            | Pote Gusto Gusto Gusto
data Gusto = Chocolate | Ddl | Pistacho | Frambuesa
data Agregado = Almendras | Rocklets
data Baño = Blanco | Negro 



--              AB ARBOLES BINARIOS          --
-- Nil = constructor 
data AB a = Nil | Bin (AB a) a (AB a) -- Arbol Binario --> Nil :: AB a

{-    CURRIFICACION   -}
max2 :: Ord a => (a, a) -> a
max2(x,y) |x>=y=x
          | otherwise = y

normaVectorial :: Floating a => (a, a) -> a
normaVectorial (x,y) = sqrt (x^2+y^2)

subtract :: Integer -> Integer -> Integer
subtract = flip (-)

{-restarUno :: t
restarUno = subtract 1-}
evaluarEnCero = \f->f 0
dosVeces = \f->f.f
flipAll = map flip


-- funcion parcial: cuando definis f definis infinitas funciones parciales
--(max3 8) :: (Ord a , Num a) => a -> a





------------------------------------------------------------11,12/03/24
{-
BUEN DISEÑO: visualizaciones, mapping, affordances
TOPDOWN , BOTTOM UP 
estrategias de procesamiento de información
        TOPDOWN suele no hacer cosas demas porque va haciendo funciones para cumplis un objetivo
        mientras que BOTTOM DOWN suele overachieve con especificaciones no demandadas.
-}

-- DATATYPES
--      data Name = Constructor1 <args> [| Constructor2 <args> ]*
data Mes = Enero | Febrero | Marzo  -- ...

data Math = Suma Int Int | Rest Int Int |
            Mult Int Int | Divi Int Int

eval :: Math -> Int
eval (Suma a b) = a + b
eval (Rest a b) = a - b
eval (Mult a b) = a * b
eval (Divi a b) = div a b

data Peano = Succ Peano | Zero -- tipo de dato algebraico Peano que representa los números naturales
        deriving Show 

incr :: Peano -> Peano
incr a = Succ a --a es otro valor de tipo Peano y Succ a representa el sucesor de a

decr :: Peano -> Peano
decr (Succ a) = a

add :: Peano -> Peano -> Peano
add Zero p = p
add (Succ n) p = Succ ( add n p ) --delegar en el lenguaje para que resuelva




------------------------------------------------------------14/03/24                    
{-   
| FOLDR   
foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b --derecha a izquierda (recursion)       
foldr F z[] = z
foldr F z (x:xs) = F x (foldr F z xs)
foldr mod 5 [7,11,8]

1:[] constructor de lista == [1]                TIENEN DISTINTA ARIDAD

| FOLDL
foldl :: Foldable t => (b -> a -> b) -> b -> t a -> b     -- izquierda -> derecha   
foldl F z[] = z
foldl F z (x:xs) = foldl F(F(zx)) xs
foldl mod 100 [35,11,5,2]
-}



-- Definir funcion partes que reciba Lista L y te devuelva Lista formada por todos los elementos en su orden de aparicion

partes :: [a] -> [[a]]

f :: a -> [[a]] -> [[a]]
f x xss = [x : ys| ys <- xss ] ++ xss -- : como constructor de lista que lo agrega al principio
partes xs = foldr f [[]] xs -- [[]] == caso base 
-- Partes [2] == [[2] []]

------------------------------------------------------------15/03/24
{- COMPOSICION DE FUNCIONES -}
prueba = (+) 3 (mod 5 3)
prueba2 = ((+) 3.mod 5) 3 --a partir del punto es lo que se calcula primero con el elemento de afuera

prueba3 = map (+ 2) [2,4]


------------------------------------------------------------19/03/24
{- RECURSION -}
--hacer uso constante del espacio de memoria

{- LISTAS -}
-- [] , x:xs

{- foldr -}
-- foldr (\each fold -> termino) neutro fuente
-- -> significa "resuelve"
sumaA = foldr (+) 0 [1,2,3] == 1+2+3+0 -- +1 ( +2 ( + 3 0 ))
restaA = foldr (-) 0 [2,3,4]

andA = foldr (&&) True 

orR = foldr (or) False

countA target = foldr (\each fold -> if target == each then fold + 1 else fold) 0
isAll target = foldr (\ each fold -> target == each && fold ) True
isAll2 target = foldr (\ each fold -> (&&) (target == each) fold) True
isAll3 target = foldr (\ each fold -> (&&) target == each ) True
isAll4 target = foldr (\ each -> (&&) $ target == each ) True


largo = foldr (\x acc -> acc + 1) 0
largo1 = foldr (\x -> (+) 1 ) 0
largo2 = (\_ -> (+) 1 ) 0

mapA0 f = foldr (\x acc -> (f x):acc) []
mapA2 f = foldr (\x acc -> (:) (f x) acc) []
mapA3 f = foldr (\x acc -> ((:).f) x acc) []
mapA4 f = foldr (\x acc -> ((:).f) x acc) []
mapA5 f = foldr ((:).f) []
mapA = (\f -> foldr ((:).f)) []
