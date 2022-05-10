import Data.List
import Data.Char

-- RESOLUCION:

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



myScore x
   | x > 90 = "A"
   | x > 80 = "B"
   | x > 70 = "C"
   | x > 60 = "D"
   | otherwise = "F"
