--1) RGB

type Color = (Int, Int, Int)


promedio:: Int -> Int -> Int
promedio x y = (x+y) `div` 2

mezclar:: Color -> Color -> Color
mezclar (r1, v1, a1) (r2, v2, a2) = (promedio r1 r2, promedio v1 v2, promedio a1 a2)

--2) linea

type Linea = (Int, [Char])

vacia :: Linea
vacia = (0, [])

moverIzq :: Linea -> Linea
moverIzq (0, cs) = (0, cs)
moverIzq (p, cs) = (p-1, cs)

moverDer :: Linea -> Linea
moverDer (p, cs) | length cs <= p = (length cs,cs)
                 | otherwise = (p+1, cs)   

moverIni :: Linea -> Linea
moverIni (p, cs) = (0, cs)

moverFin :: Linea -> Linea
moverFin (p, cs) = (length cs, cs)       

insertar :: Char -> Linea -> Linea
insertar c (p, cs) = (p+1, ins c p cs)

ins :: Char -> Int -> [Char] -> [Char]
ins c 0 cs = (c:cs)
ins c n (x:xs) = x : ins c (n-1) xs 

borrar :: Linea -> Linea
borrar (p, cs) = if p > 0 then (p - 1, take (p - 1) cs ++ drop p cs)
                          else (p,cs)



--3) CList

data CList a = EmptyCL | CUnit a | Consnoc a (CList a) a 

--a)
headCL:: CList a -> a
headCL (CUnit x) = x
headCL (Consnoc x _ _) = x
headCL _ = error "Lista Vacia!!"


tailCL:: CList a -> CList a
tailCL (CUnit _) = EmptyCL
tailCL (Consnoc _ xs _) = xs
tailCL _ = error "Lista Vacia!!"


isEmptyCL:: CList a -> Bool
isEmptyCL EmptyCL = True
isEmptyCL _ = False
 

isCUnit:: CList a -> Bool
isCUnit (CUnit _) = True
isCUnit _ = False

--b)

reverseCL :: CList a -> CList a
reverseCL cl = reverseAcc cl EmptyCL
            where 
                reverseAcc :: CList a -> CList a -> CList a
                reverseAcc EmptyCL acc = acc
                reverseAcc (CUnit x) acc = Consnoc x acc x
                reverseAcc (Consnoc x xs y) acc = reverseAcc xs (Consnoc y acc x)



--4) Aexp

data Aexp = Num Int | Prod Aexp Aexp | Div Aexp Aexp

--a)
eval :: Aexp -> Int
eval (Num n) = n
eval (Prod a1 a2) = eval a1 * eval a2 
eval (Div a1 a2) = eval a1 `div` eval a2


--b)
seval :: Aexp -> Maybe Int
seval (Num n) = Just n
seval (Prod e1 e2) = (*) <$> seval e1 <*> seval e2
seval (Div e1 e2) = do
  n1 <- seval e1
  n2 <- seval e2
  if n2 == 0 then
    Nothing
  else
    Just (n1 `div` n2)




--5) Arbol Binario

data Tree a = ET | Node (Tree a) a (Tree a) deriving Show

completo:: a -> Int -> Tree a
completo x 0 = ET
-- completo x d = Node (completo x (d-1)) x (completo x (d-1))    ESTO ESTA MAL, GENERO RECURSION DE LOS DOS LADOS
completo x d = let l = completo x (d -1)
                   in Node l x l 

balanceado :: a -> Int -> Tree a
balanceado x 0 = ET
balanceado x 1 = Node ET x ET
balanceado x n | odd n = let l = balanceado x (div (n-1) 2)
                             in Node l x l
               | otherwise = let l = balanceado x (div (n-1) 2)
                                 r = balanceado x ((div (n-1) 2) + 1)
                                 in Node l x r


--------------------------------- EJERCICIO 7 ---------------------------------
{-La definicion de member dada en teoria (la cual determina si un elemento esta en un bst)
realiza en el peor caso 2xd comparaciones, donde d es la altura del arbol. Dar una definicion
de menber que realice a lo sumo d + 1 comparaciones. Para ello definir member en terminos de
una funcion auxiliar que tenga como parametro el elemento candidato, el cual puede ser igual al
elemento que se desea buscar (por ejemplo, el ultimo elemento para el cual la comparacion de
a 6 b retorno True) y que chequee que los elementos son iguales solo cuando llega a una hoja del
arbol.-}
data Bin a = Hoja | Nodo (Bin a) a (Bin a) 

{-
2*d 
teomember a Hoja = False
teomember a (Nodo l b r) | a==b = True
                         | a < b = teomember a l
			                   | a > b = teomember a r
-}


--d+1
myMember x c Hoja = x == c
myMember x c (Nodo l y r) | x > y = myMember x c r
                          | otherwise = myMember x y l


member x Hoja = False
member x t@(Nodo l y r) = myMember x y t



--------------------------------- EJERCICIO 8 ---------------------------------
{-Definir una funcion fromOrdList :: [a ]  RBT a, que cree un red Black tree a partir de una
lista ordenada sin elementos repetidos. La funcion debe ser de orden O(n).-}

--data Color = R | B
--data RBT a = E | T Color (RBT a) a (RBT a)






--10) 

type Rank = Int
data Heap a = E | N Rank a (Heap a) (Heap a) deriving (Show)

merge :: Heap a -> Heap a -> Heap a
merge h1 E = h1
merge E h2 = h2
merge h1 @(N x a1 b1 ) h2 @(N y a2 b2 ) = if x 6 y then makeH x a1 (merge b1 h2 )
                                                   else makeH y a2 (merge h1 b2 )


rank :: Heap a -> Rank
rank E = 0
rank (N r _ _ _ ) = r


makeH x a b = if rank a > rank b then N (rank b + 1) x a b
                                 else N (rank a + 1) x b a

fromList xs = let ys = map (\x -> N 0 x E E) xs
                  pares [] = []
                  pares [x] = [x]
                  pares (x:y:hs) = merge x y : pares hs
                  g [h] = [h]
                  g hs = g (pares hs)
                  in g ys 
                  