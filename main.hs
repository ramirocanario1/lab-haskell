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

-- contar: cuenta cosas, que pueden ser caracteres, palabras o líneas. 
-- La función debe ser realizada de manera de poder recibir la opción de que contar como una función argumento
contar_chars palabra = length(map (:[]) palabra)
contar_palabras cadena = length (words cadena)

contar "lineas" lista = length lista

contar "caracteres" [] = 0
contar "caracteres" (cabeza:resto) = contar_chars cabeza + contar "caracteres" resto

contar "palabras" [] = 0
contar "palabras" (cabeza:resto) = contar_palabras cabeza + contar "palabras" resto