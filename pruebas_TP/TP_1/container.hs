module Container ( Container, newC, destinationC, netC ) -- expone esos simbolos
 where

data Container = Con String Int deriving (Eq, Show)-- (Eq, Show) para q use el igual y el show pre establecido        -- (todo desps del = == constructor)
                    -- String == su destino
newC :: String -> Int -> Container   -- construye un Contenedor dada una ciudad de destino y un peso en toneladas
newC destino num = contenedor --COMO CONSTRUIR UN NUEVO CONTAINER
    where contenedor = Con destino num

destinationC :: Container -> String  -- responde la ciuda destino del contenedor
destinationC (Con destino num) = destino --(?)

netC :: Container -> Int             -- responde el peso en toneladas del contenedor
netC (Con destino num) = num

