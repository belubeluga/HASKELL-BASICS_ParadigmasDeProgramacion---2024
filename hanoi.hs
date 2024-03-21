import GHC.Stack (pushCallStack)
data Stick = Vacio | Stack Stick Int deriving (Eq)

push :: Stick -> Int -> Stick
push Vacio d = Stack Vacio d
push (Stack s anterior) d | d < anterior = Stack s d

pop :: Stick -> Stick
pop Vacio = error "nada" -- si no lo escribo igual falla
pop (Stack s d) = s

top :: Stick -> Int
top (Stack s d) = d

data Hanoi = Hanoi Stick Stick Stick deriving (Eq)

hanoiIC :: Hanoi -> Hanoi
hanoiIC (Hanoi i c d) = Hanoi (pop i) (push c (top i)) d
--pattern matching


stickWith :: [Int] -> Stick
stickWith = foldr (\each fold -> push fold each) Vacio

initWith :: [Int] -> [Int] -> [Int] -> Hanoi
initWith i c d = Hanoi (stickWith i) (stickWith c) (stickWith d)

instance Show Hanoi
    where show (Hanoi i c d) = "I: " ++ show i ++ "\n C: " ++ show c ++ "\n D: " ++ show d

instance Show Stick
    where show s = "| " ++ printS s

printS Vacio = ""
printS (Stack s d ) = printS s ++ " " ++ show d

hanoi = initWith [] [1,3] [2]

t = [
    pop (Stack Vacio 2) == Vacio,
    pop (Stack (Stack Vacio 4) 2) == Stack Vacio 4,
    top (Stack Vacio 2) == 2,
    top (Stack (Stack Vacio 4) 2) == 2,
    -- TEST testF (pop Vacio),
    hanoiIC (Hanoi (Stack Vacio 4) Vacio Vacio) == Hanoi Vacio (Stack Vacio 4) Vacio, 
    -- TEST hanoiIC (Hanoi (Stack Vacio 4) (Stack Vacio 2) Vacio) == Hanoi Vacio (Stack (Stack Vacio 2) 4) Vacio,
    True]