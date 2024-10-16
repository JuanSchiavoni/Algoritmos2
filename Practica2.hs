import System.Win32 (COORD(yPos), xBUTTON1)
{-
1. El modelo de color RGB es un modelo aditivo que tiene al rojo, verde y azul como colores
primarios. Cualquier otro color se expresa en t´erminos de las proporciones de estos tres colores que
es necesario combinar en forma aditiva para obtenerlo. Dichas proporciones caracterizan a cada
color de manera biun´ıvoca, por lo que usualmente se utilizan estos valores como representaci´on de
un color.
Definir un tipo Color en este modelo y una funci´on mezclar que permita obtener el promedio
componente a componente entre dos colores.
-}

type Color = (Int, Int, Int)

mezclar:: Color -> Color -> Color   
mezclar (r1, g1, b1) (r2, g2, b2) = (promedio r1 r2, promedio g1 g2, promedio b1 b2)

promedio :: Int -> Int -> Int
promedio x y = (x + y) `div` 2

{-
2. Consideremos un editor de lıneas simple. Supongamos que una Lınea es una secuencia de
caracteres c1, c2, . . . , cn junto con una posicion p, siendo 0 <= p <= n, llamada cursor (consideraremos
al cursor a la izquierda de un caracter que sera borrado o insertado, es decir como el cursor de la
mayorıa de los editores). Se requieren las siguientes operaciones sobre lıneas:
vacıa :: Lınea
moverIzq :: Lınea → Lınea
moverDer :: Lınea → Lınea
moverIni :: Lınea → Lınea
moverFin :: Lınea → Lınea
insertar :: Char → Lınea → Lınea
borrar :: Lınea → Lınea
La descripcion informal es la siguiente: (1) la constante vacıa denota la lınea vacıa, (2) la ope-
racion moverIzq mueve el cursor una posicion a la izquierda (siempre que ellos sea posible), (3)
analogamente para moverDer , (4) moverIni mueve el cursor al comienzo de la lınea, (5) moverFin
mueve el cursor al final de la lınea, (6) la operacion borrar elimina el caracterer que se encuentra
a la izquierda del cursor, (7) insertar agrega un caracter en el lugar donde se encontraba el cursor
y lo mueve una posicion a la derecha.
Definir un tipo de datos Lınea e implementar las operaciones dadas.
-}

type Linea = (Int, [Char])

vacia :: Linea
vacia = (0,[])

moverIzq :: Linea -> Linea
moverIzq (0, cs) = (0, cs)
moverIzq (p, cs) = ((p-1), cs)

moverDer :: Linea -> Linea
moverDer (p, cs) | (p+1) >= length cs = ((length cs) - 1, cs) 
                 | otherwise = ((p+1), cs)

moverIni :: Linea -> Linea
moverIni (_, cs) = (0, cs)

moverFinal :: Linea -> Linea
moverFinal (_, cs) = ((length cs) -1, cs)

insertar :: Char -> Linea -> Linea
insertar c (p, cs) | (p+1) >= length cs = ((length cs), aux c (length cs) cs)
                   | otherwise = ((p+1), aux c p cs)

aux :: Char -> Int -> [Char] -> [Char]
aux c 0 xs = (c:xs)
aux c p (x:xs) = x : aux c (p-1) xs

borrar :: Linea -> Linea 
borrar (_,[]) = (0,[])
borrar (p, cs) | p >= length cs = ((p-1), take (p-1) cs)
               | p == 0 = (p, cs) 
               | otherwise = ((p-1), take (p-1) cs ++ drop p cs)


{-
3. Dado el tipo de datos
data CList a = EmptyCL | CUnit a | Consnoc a (CList a) a
a) Implementar las operaciones de este tipo algebraico teniendo en cuenta que:
Las funciones de acceso son headCL, tailCL, isEmptyCL,isCUnit.
headCL y tailCL no estan definidos para una lista vacıa.
headCL toma una CList y devuelve el primer elemento de la misma (el de mas a la
izquierda).
tailCL toma una CList y devuelve la misma sin el primer elemento.
isEmptyCL aplicado a una CList devuelve True si la CList es vacıa (EmptyCL) o False
en caso contrario.
isCUnit aplicado a una CList devuelve True sii la CList tiene un solo elemento (CUnit a)
o False en caso contrario.
b) Definir una funcion reverseCL que toma una CList y devuelve su inversa.
c) Definir una funcion inits que toma una CList y devuelve una CList con todos los posibles
inicios de la CList.
d) Definir una funcion lasts que toma una CList y devuelve una CList con todas las posibles
terminaciones de la CList.
e) Definir una funcion concatCL que toma una CList de CList y devuelve la CList con todas ellas
concatenadas

[] EmptyCL
[1] CUnit
[1,2] Consnoc 1 (EmptyCL) 2
[1,2,3] Consnoc 1 (CUnit 3) 2


-}

data CList a = EmptyCL | CUnit a | Consnoc a (CList a) a deriving (Show)

--a)
headCL :: CList a -> a
headCL (CUnit x) = x
headCL (Consnoc x _ _) = x

tailCL :: CList a -> CList a
tailCL (CUnit x) = EmptyCL
tailCL (Consnoc x y z) = Consnoc (headCL y) (tailCL y) z

isEmptyCL :: CList a -> Bool
isEmptyCL (EmptyCL) = True
isEmptyCL _ = False

isCUnitCL :: CList a -> Bool
isCUnitCL (CUnit x) = True
isCUnitCL _ = False

--b)
reverseCL :: CList a -> CList a
reverseCL (EmptyCL) = EmptyCL
reverseCL (CUnit x) = CUnit x
reverseCL (Consnoc x y z) = Consnoc z (reverseCL y) x

{-

  [] = EmptyCL
  [1] = CUnit 1 
  [1,2] = Consnoc 1 (EmptyCL) 2
  [1,2,3]= Consnoc 1 (CUnit 2) 3
  [1,2,3,4] = Consnoc 1 (Consnoc 2 EmptyCL 3) 4
  [1,2,3,4,5] = Consnoc 1 (Consnoc 2 (CUnit 3) 4) 5
-}

--c)
--init :: CList a -> CList a


{-
4. Dado el siguiente tipo algebraico:
data Aexp = Num Int | Prod Aexp Aexp | Div Aexp Aexp
a) Defina un evaluador eval :: Aexp → Int. ¿C´omo maneja los errores de divisi´on por 0?
b) Defina un evaluador seval :: Aexp → Maybe Int.
-}

data Aexp = Num Int | Prod Aexp Aexp | Div Aexp Aexp deriving (Show)

eval :: Aexp -> Int
eval (Num x) = x
eval (Prod x y) = eval x * eval y
eval (Div x y) = eval x `div` eval y

-- data Maybe a = Nothing | Just a

seval :: Aexp -> Maybe Int
seval (Num x) = Just x
seval (Prod x y) = case seval x of 
                        Nothing -> Nothing 
                        Just x -> case seval y of
                                        Nothing -> Nothing
                                        Just y -> Just (x*y) 
seval (Div x y) = case seval x of 
                        Nothing -> Nothing 
                        Just x -> case seval y of
                                        Nothing -> Nothing
                                        Just y -> if y == 0 then Nothing else Just (x `div` y)
                                
{-
5. Si un ´arbol binario es dado como un nodo con dos sub´arboles id´enticos se puede aplicar
la t´ecnica sharing, para que los sub´arboles sean representados por el mismo ´arbol. Definir las
siguientes funciones de manera que se puedan compartir la mayor cantidad posible de elementos
de los ´arboles creados:
a) completo :: a → Int → Tree a, tal que dado un valor x de tipo a y un entero d , crea un ´arbol
binario completo de altura d con el valor x en cada nodo.
b) balanceado :: a → Int → Tree a, tal que dado un valor x de tipo a y un entero n, crea un ´arbol
binario balanceado de tama˜no n, con el valor x en cada nodo.
-}

data Tree a = H | N (Tree a) a (Tree a) deriving (Show)

completo :: a -> Int -> Tree a
completo _ 0 = H
completo x d = N (completo x (d-1)) x (completo x (d-1))

balanceado :: a -> Int -> Tree a
balanceado _ 0 = H
balanceado x 1 = N H x H
balanceado x n | odd n = N (balanceado x (div (n-1) 2)) x (balanceado x (div (n-1) 2))
               | otherwise = N (balanceado x (div n 2)) x (balanceado x (div (n-1) 2))

              
balanceado2 :: a -> Int -> Tree a
balanceado2 _ 0 = H
balanceado2 x 1 = N H x H
balanceado2 x n = N izq x der
                    where
                      nodIzq = (n-1) `div` 2
                      nodDer = (n-1) - nodIzq
                      izq = balanceado2 x nodIzq
                      der = balanceado2 x nodDer

{-
6. Definir las siguientes funciones sobre arboles binarios de busqueda (bst):
1. maximum :: BST a → a, que calcula el maximo valor en un bst.
2. checkBST :: BST a → Bool , que chequea si un ´arbol binario es un bst.
-}
data BST a = Hoja | Nodo (BST a) a (BST a) deriving (Show)

maximum1 :: BST a -> a
maximum1 (Nodo Hoja a Hoja) = a
maximum1 (Nodo l a r) = maximum1 r

checkBST :: (Ord a) => BST a -> Bool
checkBST Hoja = True
checkBST (Nodo Hoja a Hoja) = True
checkBST (Nodo l a r) = left l a && right r a && checkBST l && checkBST r
                          where 
                            left Hoja _ = True
                            left (Nodo x y z) a = y < a && left x a && right z a
                            right Hoja _ = True
                            right (Nodo x y z) a = y > a && left x a && right z a

{-
7. La definici´on de member dada en teor´ıa (la cual determina si un elemento est´a en un bst)
realiza en el peor caso 2 ∗ d comparaciones, donde d es la altura del ´arbol. Dar una definici´on
de menber que realice a lo sumo d + 1 comparaciones. Para ello definir member en t´erminos de
una funci´on auxiliar que tenga como par´ametro el elemento candidato, el cual puede ser igual al
elemento que se desea buscar (por ejemplo, el ´ultimo elemento para el cual la comparaci´on de
a 6 b retorn´o True) y que chequee que los elementos son iguales s´olo cuando llega a una hoja del
´arbol.
-}

{-
8. Definir una funci´on fromOrdList :: [a ] → RBT a, que cree un red black tree a partir de una
lista ordenada sin elementos repetidos. La funci´on debe ser de orden O(n).

data Color = R | B
data RBT a = E | T Color (RBT a) a (RBT a) 

fromOrdList :: [a] -> RBT a
fromOrdList [] = E
fromOrdList xs = 
-}

{-
9. La funcion insert dada en teorıa para insertar un elemento en un rbt puede optimizarse
elimando comparaciones innecesarias hechas por la funcion balance. Por ejemplo, en la definicion
de la funcion ins cuando se aplica balance sobre el resultado de aplicar insert x sobre el subarbol
izquierdo (l ) y el subarbol derecho (r ), los casos de balance para testear que se viola el invariante
1 en el subarbol derecho no son necesarios dado que r es un rbt.
a) Definir dos funciones lbalance y rbalance que chequeen que el invariante 1 se cumple en los
subarboles izquierdo y derecho respectivamente.
b) Reemplazar las llamadas a balance en ins por llamadas a alguna de estas dos funciones.

insert :: Ord a ⇒ a → RBT a → RBT a
insert x t = makeBlack (ins x t)
              where ins x E = T R E x E
                    ins x (T c l y r) | x < y = balanceI c (ins x l) y r
                                      | x > y = balanceD c l y (ins x r)
                                      | otherwise = T c l y r
makeBlack E = E
makeBlack (T l x r) = T B l x r 
-}
data Color2 = R | B
data RBT a = E | T Color2 (RBT a) a (RBT a)

balanceI :: Color2 -> RBT a -> a -> RBT a -> RBT a
balanceI B (T R (T R a x b) y c) z d = T R (T B a x b) y (T B c z d)
balanceI B (T R a x (T R b y c)) z d = T R (T B a x b) y (T B c z d)
balanceI c l a r = T c l a r

balanceD :: Color2 -> RBT a -> a -> RBT a -> RBT a
balanceD B a x (T R (T R b y c) z d) = T R (T B a x b) y (T B c z d)
balanceD B a x (T R b y (T R c z d)) = T R (T B a x b) y (T B c z d)
balanceD c l a r = T c l a r

{-
10. Definir una funcion fromList :: [a ] → Heap a, que cree un leftist heap a partir de una lista,
convirtiendo cada elemento de la lista en un heap de un solo elemento y aplicando la funcion merge
hasta obtener un solo heap. Aplicar la funcion merge [lg n] veces, donde n es la longitud de la
lista que recibe como argumento la funcion, de manera que fromList sea de orden O(n).
-}

type Rank = Int
data Heap a = E2 | N2 Rank a (Heap a) (Heap a) deriving Show

fromList :: Ord a => [a] -> Heap a
fromList xs = let ys = map (\x -> N2 0 x E2 E2) xs
                  pares [] = []
                  pares [x] = [x]
                  pares (x:y:zs) = merge x y : pares zs
                  junta [h] = h
                  junta hs = junta (pares hs)
                  in junta ys 

merge :: Ord a => Heap a -> Heap a -> Heap a
merge h1 E2 = h1
merge E2 h2 = h2
merge h1@(N2 _ x a1 b1) h2@(N2 _ y a2 b2) = if x <= y then makeH x a1 (merge b1 h2)
                                               else makeH y a2 (merge h1 b2)

rank :: Heap a -> Rank
rank E2 = 0
rank (N2 r _ _ _) = r
makeH x a b = if rank a > rank b then N2 (rank b + 1) x a b
else N2 (rank a + 1) x b a


{-
[2,6,1,3,5,4]
[N2 0 2 E2 E2,
N2 0 6 E2 E2,
N2 0 1 E2 E2,
N2 0 3 E2 E2,
N2 0 5 E2 E2,
N2 0 4 E2 E2]

merge (N2 0 4 E2 E2) (merge (N2 0 5 E2 E2) (merge (N2 0 3 E2 E2) (merge (N2 0 1 E2 E2) (merge (N2 0 2 E2 E2) (N2 0 6 E2 E2)))))

[merge (N2 0 2 E2 E2) (N2 0 6 E2 E2), merge (N2 0 1 E2 E2) (N2 0 3 E2 E2), merge (N2 0 5 E2 E2) (N2 0 4 E2 E2)]
[ ((N2 0 2 E2 E2), (N2 0 6 E2 E2))  ,  ((N2 0 1 E2 E2), (N2 0 3 E2 E2)), ((N2 0 5 E2 E2), (N2 0 4 E2 E2))]
[merge ((N2 0 2 E2 E2), (N2 0 6 E2 E2)) ((N2 0 1 E2 E2), (N2 0 3 E2 E2)), ((N2 0 5 E2 E2), (N2 0 4 E2 E2))]
[((N2 0 1 E2 E2), (N2 0 2 E2 E2), (N2 0 3 E2 E2), (N2 0 6 E2 E2))  , ((N2 0 5 E2 E2), (N2 0 4 E2 E2))]
[merge ((N2 0 1 E2 E2), (N2 0 2 E2 E2), (N2 0 3 E2 E2), (N2 0 6 E2 E2))  , ((N2 0 5 E2 E2), (N2 0 4 E2 E2))]
-} 


