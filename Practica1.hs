{-
1. Definir las siguientes funciones en forma recursiva:
a) borrarUltimo que dada una lista borra el  ́ultimo elemento de la lista. No utilizar reverse, ni
tail.
-}

borrarUltimo :: [a] -> [a]
borrarUltimo [] = []
borrarUltimo [x] = []
borrarUltimo (x:xs) = x: borrarUltimo xs


{-
b) collect :: [(k , v )] → toma un lista de pares (clave, valor) y asocia cada clave  ́unica con todos
los valores con los que estaba apareada originalmente. Por ejemplo: collect
-}



{-
c) serie que se comporta de la siguiente manera: serie [1, 2, 3] = [[ ], [1], [1, 2], [1, 2, 3]] Dar su
tipo m ́as general.
-}

serie:: [a] -> [[a]]
serie [] = [[]]
serie xs = serie (init xs) ++ [xs] -- init devuelve la lista sin el ultimo elemento



{-
d) paresIguales :: Int → Int → Int → Int → Bool toma 4 n ́umeros enteros y retorna True si de
dos en dos son iguales (en cualquier orden), en los dem ́as casos retorna False. Por ejemplo:
paresIguales 3 1 1 2 = False paresIguales 3 1 3 1 = True paresIguales 3 3 1 1 = True
paresIguales 3 1 1 3 = True 
-}

paresIguales :: Int -> Int -> Int -> Int -> Bool
paresIguales a b c d = (a == b && c == d) || (a == c && b == d) || (a == d && b == c)


{-
e) isosceles :: Int → Int → Int → Bool que dadas la longitud de los lados de un tri ́angulo nos
dice si es un tri ́angulo is ́osceles. 
-}

isosceles :: Int -> Int -> Int -> Bool
isosceles x y z = (x == y) || (x == z) || (y == z)



{-
f ) ror que dada una lista xs y un entero n, tal que n 6 lenght xs, rota los primeros n elementos
de xs a la derecha: ror 3 [1, 2, 3, 4, 5] = [4, 5, 1, 2, 3]. Definir una versi ́on recursiva de ror ,
sin usar drop, take ni tail. -}

ror :: Int -> [a] -> [a]
ror n xs = ror' n xs xs
        where 
            ror' _ _ [] = []
            ror' 0 ys zs = zs ++ ys
            ror' m (y:ys) (z:zs) = ror' (m - 1) ys (zs ++ [y])


{-
g) upto :: Int → Int → [Int] que dado dos n ́umeros enteros n y m devuelve la lista [n, n + 1, n +
2, ..., m ] en caso que n 6 m y la lista [ ] en otro caso. No usar listas por comprensi ́on.
-}

upto :: Int -> Int -> [Int]
upto n m | n > m = []
         | otherwise = n : upto (n + 1) m


{-
h) eco que devuelve la cadena obtenida a partir de la cadena xs repitiendo cada elemento tantas
veces como indica su posici ́on. No usar listas por comprensi ́on.
Por ejemplo: eco "hola" = "hoolllaaaa"
-}

eco :: [a] -> [a]
eco xs = concat (zipWith replicate [1..] xs)


{-
2. Definir usando listas por comprensi ́on las funciones:
a) cambios : [a ] → [Int], que dada una lista, devuelve la lista de los  ́ındices en que la lista
cambia. Es decir, dada la lista s retorna la lista con los i tal que si 6 = si+1
cambios [1, 1, 1, 3, 3, 1, 1] = [2, 4] 
-}

cambios :: Eq a => [a] -> [Int]
cambios xs = [i | (i, (x, y)) <- zip [1..] (zip xs (tail xs)), x /= y]


{-
b) oblongoNumber :: [Int] que genera la lista de los n ́umeros oblongos. Un n ́umero es oblongo
si es el producto de dos naturales consecutivos. Por ejemplo, los n ́umeros [2, 6, 12, 20, ...]
-}

oblongoNumber :: [Int]
oblongoNumber = [n * (n + 1) | n <- [1..]]


{-
c) abundantes :: [Integer] que es la lista de todos los n ́umeros abundantes. Un n ́umero natural
n se denomina abundante si es menor que la suma de sus divisores propios. Por ejemplo, 12
y 30 son abundantes pero 5 y 28 no lo son. Por ejemplo abundates = [12, 18, 20, 24, 30, 36, ...
-}

abundantes :: [Integer]
abundantes = [n | n <- [1..100], n < sum (divisoresPropios n)]
  where
    divisoresPropios n = [x | x <- [1..(n-1)], n `mod` x == 0]



{-

e) euler :: Int → Int tal que euler n es la suma de todos los m ́ultiplos de 3  ́o 5 menores que n.
Por ejemplo, euler 10 = 23. Puedes usar sin definir la funci ́on sum que suma los elementos
de una lista.-}

euler :: Int -> Int 
euler n = sum [x | x <- [1.. n-1], x `mod` 3 == 0 || x `mod` 5 == 0]


{-
f ) expandir :: [Int] → [Int] que reemplace en una lista de numeros positivos cada numero n por
n copias de sı mismo:
Ejemplo: expandir [3, 4, 2] = [3, 3, 3, 4, 4, 4, 4, 2, 2]
-}

expandir :: [Int] -> [Int]
expandir xs = concat [[x | _ <- [1..x]] | x <- xs]


{-
3. Dar dos ejemplos de funciones que tengan los siguientes tipos:
a) (Int → Int) → (Bool → Bool) 
b) Bool → (Int → Bool)
c) Char → Char
d) Int → (Int → Bool) → [Int]
e) [a ] → (a → [b ]) → [b ]
f ) [[a ]] → (a → Bool) → [a ]
g) (a, b, c) → Bool
h) (a, b, c) → Int → c
i) (a, a, a) → Int → a -}

{-
4. Dar el tipo de la siguiente funciones o expresiones:

a) foo1 p = if p then (p ∧) else (p ∧)
Tipo Bool

b) foo2 x y z = x (y z)
Tipo (b->c) -> (a->b) -> a -> c

c) foo3 x y z = x y z
(c -> b -> a) -> c

d) foo4 x y z = x y : z
error!!!

e) foo5 x y z = x : y z
error!!!

f ) foo6 x y z = x ++ y z
error!!

g) foo7 a b = if b a then head a else [ ]
[a] -> (a -> Bool) -> [a]

h) foo8 a b = if b a then a else []
[a] -> (a -> Bool) -> [a]

i) foo9 a b = if b a then head (:a) else (:[ ]) 
[a] -> (a -> Bool) -> [a]


-}


{-
5. Definir las siguientes funciones usando foldr:
a) map :: (a → b) → [a ] → [b ] que dada una funci ́on y una lista, aplica la funci ́on a cada
elemento de la lista. 
-}

map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr (\x acc -> f x : acc) [] xs


{-
b) filter :: (a → Bool) → [a ] → [a ] , que dado un predicado y una lista xs, devuelve una lista
con los elementos de xs que satisfacen el predicado.
-} 




{-
c) unzip ::[(a, b)] → ([a ], [b ]), que dada una lista de tuplas xs retorna una tupla de listas donde
cada una corresponde a los primeros y secundos elementos de los pares respectivamente.
Ej. unzip [(’a’, 1), (’z’, 7), (’h’, 9)] = ("azh", [1, 7, 9])
-}




{-
d) pair2List ::(a, [b ]) → [(a, b)] que dado un par formado por un valor x y una lista xs convierta
a la lista xs en una lista de pares, formada con los elementos de xs y x .
Ej. pair2List (x , [y1 , y2 , y3 ]) = [(x , y1 ), (x , y2 ), (x , y3 )]
e) maxSec :: [(Int, Int)] → (Int, Int), que dada una lista de pares de naturales que represente a
una lista de segmentos de la recta, calcule el segmento m ́as largo de la misma.
Ej.maxSec [(1, 2), (0, 7), (4, 6)] = (0, 7)
Puede definir una funci ́on auxiliar maxL :: (Int, Int) → (Int, Int) → (Int, Int), que dados dos
pares de naturales que representan a dos segmentos de la recta, devuelva el segmento cuya
longitud sea m ́axima.
Ej.maxL (1, 2) (0, 7) = (0, 7). -}



