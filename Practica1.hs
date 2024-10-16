{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Avoid lambda" #-}
import System.Win32 (COORD(xPos))
--1. Definir las siguientes funciones en forma recursiva:

--a) borrarUltimo que dada una lista borra el ´ultimo elemento de la lista. No utilizar reverse, ni
--tail.
borrarUltimo :: [a] -> [a]
borrarUltimo [] = []
borrarUltimo [_] = []
borrarUltimo (x:xs) = x : borrarUltimo xs

--b) collect :: [(k, v)] → toma un lista de pares (clave, valor) y asocia cada clave ´unica con todos
--los valores con los que estaba apareada originalmente. Por ejemplo: collect
-- --HECHO por Rabacho en clases
borrar h [] = []
borrar h ((a, e):xs) | h == a = borrar h xs 
                     | otherwise = (a,e): borrar h xs 

klave :: Eq t => t -> [(t, a)] -> [a]
klave h [] = []
klave h ((a, e) : xs) | h == a = e : klave h xs 
                      | otherwise = klave h xs 

collect :: Eq k => [(k , v)] -> [(k,[v])]
collect [] = []
collect ((a,e):xs) = (a,e : klave a xs) : collect (borrar a xs)

--Hecho por el cra Andres Grillo en clase
ins :: Eq k => (k,v) -> [(k, [v])] -> [(k, [v])]
ins (k,v) [] = [(k,[v])]
ins (k,v) ((k',vs):xs) | k == k' = (k', v : vs) : xs
                       | otherwise = (k', vs): ins (k,v) xs 

collect' [] = []
collect' ((k,v):xs) = ins (k,v) (collect' xs)

--c) serie que se comporta de la siguiente manera: serie [1, 2, 3] = [[ ], [1], [1, 2], [1, 2, 3]] Dar su
--tipo mas general.
serie :: [a] -> [[a]]
serie [] = [[]]
serie xs = serie (init xs) ++ [xs]

--d) paresIguales :: Int → Int → Int → Int → Bool toma 4 n´umeros enteros y retorna True si de
--dos en dos son iguales (en cualquier orden), en los dem´as casos retorna False. Por ejemplo:
--paresIguales 3 1 1 2 = False paresIguales 3 1 3 1 = True paresIguales 3 3 1 1 = True
--paresIguales 3 1 1 3 = True
paresIguales :: Int -> Int -> Int -> Int -> Bool
paresIguales a b c d = paresAux [a,b,c,d]

paresAux [] = False
paresAux (x:xs) | ocurre x xs == 2 = True
                | otherwise = paresAux xs

ocurre _ [] = 0
ocurre y (x:xs) | y == x = 1 + ocurre y xs
                | otherwise = ocurre y xs

--e) isosceles :: Int → Int → Int → Bool que dadas la longitud de los lados de un tri´angulo nos
--dice si es un tri´angulo is´osceles.
isosceles :: Int -> Int -> Int -> Bool
isosceles a b c = hayDuplicados [a,b,c]

hayDuplicados :: (Eq a) => [a] -> Bool
hayDuplicados [] = False
hayDuplicados (x:xs) = elementoEnLista x xs || hayDuplicados xs

elementoEnLista :: (Eq a) => a -> [a] -> Bool
elementoEnLista _ [] = False
elementoEnLista y (z:zs) = y == z || elementoEnLista y zs

--f) ror que dada una lista xs y un entero n, tal que n <= lenght xs, rota los primeros n elementos
--de xs a la derecha: ror 3 [1, 2, 3, 4, 5] = [4, 5, 1, 2, 3]. Definir una versi´on recursiva de ror ,
--sin usar drop, take ni tail.
ror :: [a] -> Int -> [a]
ror xs n = ror' n xs xs

ror' _ _ [] = []
ror' 0 ys zs = zs 
ror' m (y:ys) (z:zs) = ror' (m - 1) ys (zs ++ [y])

rorSimple n xs = drop n xs ++ take n xs 

--g) upto :: Int → Int → [Int] que dado dos n´umeros enteros n y m devuelve la lista [n, n + 1, n +
--2, ..., m ] en caso que n 6 m y la lista [ ] en otro caso. No usar listas por comprensi´on.
upto :: Int -> Int -> [Int]
upto n m = if n>m then [] else n : upto (n+1) m

upto2 n m | n>m = []
          | otherwise = n : upto2 (n+1) m

--h) eco que devuelve la cadena obtenida a partir de la cadena xs repitiendo cada elemento tantas
--veces como indica su posici´on. No usar listas por comprensi´on.
--Por ejemplo: eco "hola" = "hoolllaaaa"
eco :: [a] -> [a]
eco xs = multiplicar (zip xs [1..])

multiplicar :: [(a, Int)] -> [a]
multiplicar [] = []
multiplicar (x:xs) = accionar x ++ multiplicar xs

accionar :: (a, Int) -> [a]
accionar (x,n) = replicate n x


--2. Definir usando listas por comprensi´on las funciones:
--a) cambios : [a ] → [Int], que dada una lista, devuelve la lista de los ´ındices en que la lista
--cambia. Es decir, dada la lista s retorna la lista con los i tal que si != si+1
--cambios [1, 1, 1, 3, 3, 1, 1] = [2, 4]
cambios :: Eq a => [a] -> [Int]
cambios [] = []
cambios xs = [i | (i, (x,y)) <- zip [0..] (zip xs (tail xs)), x /= y]


--b) oblongoNumber :: [Int] que genera la lista de los n´umeros oblongos. Un n´umero es oblongo
--si es el producto de dos naturales consecutivos. Por ejemplo, los n´umeros [2, 6, 12, 20, ...]
oblongoNumber :: [Int]
oblongoNumber = [n * (n+1) | n <- [0..], n<30]


--c) abundantes :: [Integer] que es la lista de todos los n´umeros abundantes. Un n´umero natural
--n se denomina abundante si es menor que la suma de sus divisores propios. Por ejemplo, 12
--y 30 son abundantes pero 5 y 28 no lo son. Por ejemplo abundates = [12, 18, 20, 24, 30, 36, ...
abundantes :: Int -> [Int]
abundantes x = [n | n <- [0..x], sum(divisoresPropios n) > n]

divisoresPropios :: Int -> [Int]
divisoresPropios n = [x | x <- [1..n-1], n `mod` x == 0]


--d) eco que devuelve la cadena obtenida a partir de la cadena xs repitiendo cada elemento tantas
--veces como indica su posici´on. Por ejemplo: eco "hola" = "hoolllaaaa"
eco' :: [Char] -> [Char]
eco' [] = []
eco' xs = concat [replicate i c | (c,i) <- zip xs [1..]]


--e) euler :: Int → Int tal que euler n es la suma de todos los m´ultiplos de 3 ´o 5 menores que n.
--Por ejemplo, euler 10 = 23. Puedes usar sin definir la funci´on sum que suma los elementos
--de una lista.
--euler :: Int -> Int
--euler n = sum [x | x <- [1..(n-1)], x `mod` 5 == 0 || x `mod` 3 == 0] 


--f) expandir :: [Int] → [Int] que reemplace en una lista de n´umeros positivos cada n´umero n por
--n copias de s´ı mismo:
--Ejemplo: expandir [3, 4, 2] = [3, 3, 3, 4, 4, 4, 4, 2, 2]
expandir :: [Int] -> [Int]
expandir xs = concat [replicate x x | x <- xs] 


--5. Definir las siguientes funciones usando foldr:
--a) map :: (a → b) → [a ] → [b ] que dada una funci´on y una lista, aplica la funci´on a cada
--elemento de la lista.
--map' :: (a -> b) -> [a] -> [b]
--map' f (x:xs) = foldr (f) xs
--map' f xs = foldr (f) xs

--b) filter :: (a → Bool) → [a ] → [a ] , que dado un predicado y una lista xs, devuelve una lista
--con los elementos de xs que satisfacen el predicado.
filter' :: (a -> Bool) -> [a] -> [a]
filter' p xs = foldr (\x acc -> if p x then x : acc else acc) [] xs


