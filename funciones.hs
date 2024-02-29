{- EJERCICIO 2 -}
import Data.Maybe
import Data.Char
import GHC.Real

valorAbsoluto :: Float -> Float
valorAbsoluto x |x < 0 = -x --valores negativos entre parentesis
                 |otherwise = x

-- divisble x 4 o 400 pero no x 100


bisiesto :: Int -> Bool
bisiesto x | (mod x 4 == 0 || mod x 400 == 0) && (mod x 100 /= 0) = True
           |otherwise = False
        
