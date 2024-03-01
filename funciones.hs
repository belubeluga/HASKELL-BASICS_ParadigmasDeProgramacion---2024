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

inverso :: Float -> Maybe Float
inverso 0 = Nothing
inverso x = Just (1/x)