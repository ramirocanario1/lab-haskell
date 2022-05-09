-- tac: revierte el orden de líneas de un archivo, es decir, devuelve el archivo en el orden inverso
tac [] = []
tac (x:xs) = tac xs ++ [x]