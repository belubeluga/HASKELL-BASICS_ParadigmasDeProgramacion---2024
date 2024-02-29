------------------------------------------------------------29/02/24
import Data.Maybe
import Data.Char

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

-- EJERCICIO 2
