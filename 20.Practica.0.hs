module Practica0 where

import Data.List
import Data.Char

{-
1) Los siguientes códigos tienen errores, cargar el archivo 20.Practica.0.hs en el interprete de Haskell
GHCi, leer los mensajes de error y corregirlos hasta que el archivo se cargue correctamente.
-}

-- a)
regla b = case b of
    True -> "Quedate en Casa"
    False -> "Qudate en Casa"

-- b)
cases [x] = []
cases (x:y:xs) = y:cases(x:xs)
cases [] = []

-- c)
myMap f [] = []
myMap f (x:xs)= f x : myMap f xs

-- d)
listNumeros = 1 : 2 : 3 : []

-- e)
hola [] ys = ys
hola (x:xs) ys = x : xs ++ ys

-- f)
addToTail x xs = map (+x) (tail xs)

-- g)
--listmin xs = head . sort xs

-- h) (*)
smap f [] = []
smap f [x] = [f x]
smap f (x:xs) = f x : smap f xs



-- 2. Definir las siguientes funciones y determinar su tipo:

-- a) five, que dado cualquier valor, devuelve 5
five :: a -> Int
five x = 5

-- b) apply, que toma una función y un valor, y devuelve el resultado de
-- aplicar la función al valor dado
apply :: (a -> b) -> a -> b
apply f x = f x  

-- c) identidad, la función identidad
identidad :: a -> a
identidad x = x

-- d) first, que toma un par ordenado, y devuelve su primera componente
first :: (a,b) -> a
first (x,_) = x

-- e) derive, que aproxima la derivada de una función dada en un punto dado
derive:: (Float -> Float) -> Float -> Float -> Float
derive f x h = (f (x+h) - f x) / h

-- f) sign, la función signo
signo:: Int -> Int
signo x | x < 0 = -1
        | x > 0 = 1
        | otherwise = 0

-- g) vabs, la función valor absoluto (usando sign y sin usarla)
vabs:: Int -> Int
vabs x | x > 0 = x 
       | x < 0 = -x
       | x == 0 = 0

vabsConSign:: Int -> Int
vabsConSign x | signo x == 1 = x
              | signo x == -1 = -x
              | otherwise = 0

-- h) pot, que toma un entero y un número, y devuelve el resultado de
-- elevar el segundo a la potencia dada por el primero

pot:: Int -> Int -> Int
pot x y = x^y

{-i) xor, el operador de disyunción exclusiva
    F F - F
    F V - V
    V F - V
    V V - F

    xor' False False = False
    xor' True False = True
    xor' False True = True
    xor' True True = False

    xor' False False = False
    xor' True True = False
    xor' _ _ = True
-}

xor:: (Eq a) => a -> a -> Bool
xor x y | x == y = False
        | otherwise = True

-- j) max3, que toma tres números enteros y devuelve el máximo entre llos
max3:: Int -> Int -> Int -> Int
max3 x y z = max x (max y z)

-- k) swap, que toma un par y devuelve el par con sus componentes invertidas
swap:: (a, b) -> (b, a)
swap (x, y) = (y, x)



{-
3) Definir una función que determine si un año es bisiesto o no, de
acuerdo a la siguiente definición:

año bisiesto 1. m. El que tiene un día más que el año común, añadido al mes de febrero. Se repite
cada cuatro años, a excepción del último de cada siglo cuyo número de centenas no sea múltiplo
de cuatro. (Diccionario de la Real Academia Espaola, 22ª ed.)

1900
2000
2100

¿Cuál es el tipo de la función definida?
-}

--kk a = (mod a 400 == 0) || (mod a 400 == 0) && (not(mod a 100 == 0))

esBisiesto :: Int -> Bool
esBisiesto anio = (esMultiploDe 4 anio) && ((not (esMultiploDe 100 anio)) || (esMultiploDe 400 anio))

esMultiploDe :: Int -> Int -> Bool
esMultiploDe divisor dividendo = (dividendo `mod` divisor) == 0


{-
4) Dar al menos dos ejemplos de funciones que tengan cada uno de los siguientes tipos:
-}

--a) (Int -> Int) -> Int
fa:: (Int -> Int) -> Int
fa f = (f 3) + 2
fa' g = if (g 8) == 1 then 0 else 4
fa'' g | (g 8) == 1 = 0
       | otherwise = 4 


--b) Int -> (Int -> Int)
fb::Int -> Int -> Int
fb x y = x + y
fb' x y | x == 1 = 1
        | y == 4 = 5
        | otherwise = error "Los valores ingresados no son validos"


--c) (Int -> Int) -> (Int -> Int)
mif:: Int -> Int -> Int -> Int
mif x y z = x * y * z


--d) Int -> Bool
esPar:: Int -> Bool
esPar n = n `mod` 2 == 0


--e) Bool -> (Bool -> Bool)
andBool:: Bool -> Bool -> Bool
andBool True True = True
andBool _ _ = False


--f) (Int,Char) -> Bool
ff:: (Int, Char) -> Bool
ff (v, c) = if c == 'a' then even v else even (v+1)


--g) (Int,Int) -> Int
sumaCoord:: (Int, Int) -> Int
sumaCoord (x, y) = x + y


--h) Int -> (Int,Int)
divi2:: Int -> (Int, Int)
divi2 n = (n `div` 2, n `mod` 2)


--i) a -> Bool
es3::(Eq a, Num a)=> a -> Bool
es3 x = x == 3


--j) a -> a
myIdentidad:: a -> a
myIdentidad x = x



{-
5) Definir las siguientes funciones usando listas por comprensión:

a) 'divisors', que dado un entero positivo 'x' devuelve la
lista de los divisores de 'x' (o la lista vacía si el entero no es positivo)
-}

divisors:: Int -> [Int]
divisors x = [ n | n <- [1..x], x `mod` n == 0 ]


{-
b) 'matches', que dados un entero 'x' y una lista de enteros descarta
de la lista los elementos distintos a 'x'
-}

matches :: Int -> [Int] -> [Int]
matches x xs = [y | y <- xs, y == x]


{-
c) 'cuadrupla', que dado un entero 'n', devuelve todas las cuadruplas
'(a,b,c,d)' que satisfacen a^2 + b^2 = c^2 + d^2,
donde 0 <= a, b, c, d <= 'n'
-}

cuadrupla:: Int -> [(Int, Int, Int, Int)]
cuadrupla n = [(a,b,c,d) | a <- [0..n], b <- [0..n], c <- [0..n], d <- [0..n], a^2 + b^2 == c^2 + d^2]


{-
(d) 'unique', que dada una lista 'xs' de enteros, devuelve la lista
'xs' sin elementos repetidos
unique :: [Int] -> [Int]
-}

unique' mesa [] = mesa
unique' mesa mano@(x:xs) = if elem x mesa then unique' mesa xs
                                          else unique' (mesa++[x]) xs

unique :: [Int] -> [Int]
unique xs = unique' [] xs


{-
6) El producto escalar de dos listas de enteros de igual longitud
es la suma de los productos de los elementos sucesivos (misma
posición) de ambas listas.  Definir una función 'scalarProduct' que
devuelva el producto escalar de dos listas.

Sugerencia: Usar las funciones 'zip' y 'sum'. -}

scalarProduct:: [Int] -> [Int] -> Int
scalarProduct xs ys = sum [ x * y | (x,y) <- zip xs ys ]



{-
7) Sin usar funciones definidas en el
preludio, defina recursivamente las siguientes funciones y
determine su tipo más general:
-}

{-
a) 'suma', que suma todos los elementos de una lista de números
-}

suma:: [Int] -> Int
suma [] = 0
suma (x:xs) = x + suma xs


--8)
suma' xs = fold (+) 0 xs


{-
b) 'alguno', que devuelve True si algún elemento de una
lista de valores booleanos es True, y False en caso
contrario
-}

alguno:: [Bool] -> Bool
alguno [] = False
alguno (x:xs) = x || alguno xs


--8)
alguno' xs = fold (||) False xs


{-
c) 'todos', que devuelve True si todos los elementos de
una lista de valores booleanos son True, y False en caso
contrario
-}

todos:: [Bool] -> Bool
todos [] = True
todos [x] = x
todos (x:xs) = x && todos xs


--8)
todos' xs = fold (&&) True xs


{-
d) 'codes', que dada una lista de caracteres, devuelve la
lista de sus ordinales
-}
code c = buscar c (zip (['a'..'m']++['ñ']++['o'..'z']) [1..])
buscar c [] = error "El caracter no tiene ordinal"
buscar c ((x,i):xs) = if x == c then i else buscar c xs


codes:: [Char] -> [Int]
codes [] = []
codes (x:xs) = code x : codes xs


--8)
codes' xs = maps code xs

{-
e) 'restos', que calcula la lista de los restos de la
división de los elementos de una lista de números dada por otro
número dado
-}

divInt' _ 0 _ = error "Division por cero"
divInt' x n d | d * n < x = divInt' x n (d-1)
              | d * n == x = d
              | otherwise = (d-1)

divInt x n = divInt' x n 1

resto n x = x - (divInt x n) * n 

restos _ [] = []
restos n (x:xs) = resto x n : restos n xs


--8)
--restos' = map (resto n) xs


{-
f) 'cuadrados', que dada una lista de números, devuelva la
lista de sus cuadrados
-}

cuadrados:: [Int] -> [Int]
cuadrados [] = []
cuadrados (x:xs) = x ^ 2 : cuadrados xs


--8)
cuadrados' xs = map (^2) xs


{-
g) 'longitudes', que dada una lista de listas, devuelve la
lista de sus longitudes
-}

longitudes:: [[a]] -> [Int]
longitudes [] = []
longitudes (x:xs) = length x :longitudes xs


--8)
longitudes' xss = map (length) xss


{-
h) 'orden', que dada una lista de pares de números, devuelve
la lista de aquellos pares en los que la primera componente es
menor que el triple de la segunda
-}

orden:: [(Int, Int)] -> [(Int, Int)]
orden [] = []
orden ((x,y):xs) = if x < y*3 then (x,y):orden xs
                              else orden xs


--8)
orden' xs = filters (\(x,y)->x<3*y) xs

{-
i) 'pares', que dada una lista de enteros, devuelve la lista
de los elementos pares
-}

pares:: [Int] -> [Int]
pares [] = []
pares (x:xs) = if (even x) then  x:pares xs
                           else pares xs
                               

--8)
pares' xs = filters even xs


{-
j) 'letras', que dada una lista de caracteres, devuelve la
lista de aquellos que son letras (minúsculas o mayúsculas)
-}

letras::[Char] -> [Char]
letras [] = []
letras (x:xs) = if x `elem` ['a'..'z'] then x: letras xs
                                       else letras xs


--8)



{-
k) 'masDe', que dada una lista de listas 'xss' y un
número 'n', devuelve la lista de aquellas listas de 'xss'
con longitud mayor que 'n' 
-}

masDe [] _ = []
masDe (xs:xss) n = if length xs > n then xs : masDe xss n
                                     else masDe xss n


--8)


{-
8) Redefinir las funciones del ejercicio anterior usando foldr, map y filter.
ver su definición en https://hoogle.haskell.org/
-}

fold f e [] = e
fold f e (x:xs) = f x (fold f e xs)


maps f [] = []
maps f (x:xs) = f x : maps f xs


filters p [] = []
filters p (x:xs) = if (p x) then x : filters p xs
                            else filters p xs