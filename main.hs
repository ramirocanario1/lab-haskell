import Data.List
import Data.Char

----------------------------------- FASE 1 -----------------------------------

-- tac: revierte el orden de líneas de un archivo, es decir, devuelve el archivo en el orden inverso
tac x = reverse x

-- rev: revierte las cadenas que componen el archivo
rev x = map reverse x

-- quitar: quita la ocurrencia de una línea dada.
quitar x lista = delete x lista

-- contar: cuenta cosas, que pueden ser caracteres, palabras o líneas. 
-- La función debe ser realizada de manera de poder recibir la opción de que contar como una función argumento
contar_chars palabra = length(map (:[]) palabra)
contar_palabras cadena = length (words cadena)

contar "lineas" lista = length lista

contar "caracteres" [] = 0
contar "caracteres" (cabeza:resto) = contar_chars cabeza + contar "caracteres" resto

contar "palabras" [] = 0
contar "palabras" (cabeza:resto) = contar_palabras cabeza + contar "palabras" resto

-- a_titulos: cambia todas las líneas de un archivo a formato de título. Se considera formato de
-- título a una cadena cuyas palabras de más de 4 letras comienza en mayúscula.
a_titulo palabra
  | length palabra > 4 = [toUpper (head palabra)] ++ tail palabra
  | otherwise = palabra

separar_palabras linea = unwords (map a_titulo (words linea))

a_titulos archivo = map separar_palabras archivo

-- limpiar: quitar los espacios al principio y al final de las líneas de un archivo.
quitar_espacios linea = dropWhile isSpace (dropWhileEnd isSpace linea)

limpiar archivo = map quitar_espacios archivo

----------------------------------- FASE 2 -----------------------------------

----------- CODIFICACIÓN -----------

get_arreglo linea = map length (words linea) -- Arreglo del paso 2

separar_cada n [] = [] -- Separacion del paso 4
separar_cada n linea = take n linea : separar_cada n (drop n linea) 

separar [] [] = [] -- Separacion a partir de arreglo (paso 7)
separar linea (x:xs) = take x linea : separar (drop x linea) xs

-- Función intermedia para mejor legibilidad
procesar linea = concat (map reverse (separar_cada 4 (concat (words linea))))

codificar linea = unwords (separar (procesar linea) (get_arreglo linea))

----------- DECODIFICACIÓN -----------

hallar_arreglo [] = [] 
hallar_arreglo linea = length (head (words linea)) : hallar_arreglo (unwords (tail (words linea)))

decodificar linea = unwords (separar (concat (map reverse (separar_cada 4 (concat (words (linea)))))) (hallar_arreglo linea))

-- Funcion para probar la decodificación.
probar linea = decodificar (codificar linea)
