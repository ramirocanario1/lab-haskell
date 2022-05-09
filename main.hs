-- UTILS:

-- funcion que invierte una cadena
invertir :: [a] -> [a]  
invertir = foldl (\acc x -> x : acc) []



-- RESOLUCION:

-- tac: revierte el orden de líneas de un archivo, es decir, devuelve el archivo en el orden inverso
tac [] = []
tac (x:xs) = tac xs ++ [x]

-- rev: revierte las cadenas que componen el archivo
rev [] = []
rev (x) = map invertir x

-- quitar: quita la ocurrencia de una línea dada.
quitar _ [] = []
quitar x (cabeza:resto) | x == cabeza = resto
quitar x (y:resto) = y: quitar x resto

