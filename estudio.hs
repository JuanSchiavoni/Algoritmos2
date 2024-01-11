import Prelude


--clase 1 Sintaxis Haskell 21/3


-- The layout rule
fac 0 = 1
fac n = n * fac(n - 1)


longitud [] = 0
longitud (x:xs) = 1 + longitud xs 


-- Funciones
add::(Int,Int) -> Int
add (x,y) = x + y


zeroto::Int -> [Int]
zeroto n = [0..n]


-- Currificacion
mult::Int -> (Int -> (Int -> Int))
mult x y z = x * y * z


-- Expresiones condicionales
mySignum :: Int -> Int
mySignum n = if n < 0 then (-1) else 
           if n == 0 then 0 else 1


-- Ecuaciones con Guardas
mySignum' n | n <  0 = (-1)
            | n == 0 = 0
            | otherwise = 1 


-- Pattern Matching y Patrones de listas
tail :: [a] -> [a] 
tail (_:xs) = xs



-- Clase 2 Tipos Haskell 11/04



-- type

type Pos = (Int, Int)

origin     ::  Pos
origin     = (0,0)

left       :: Pos -> Pos
left (x,y) = (x-1, y)



-- tipo con parametros

type Par a = (a, a)

multi :: Par Int -> Int
multi (x,y) = x * y

copy :: a -> Par a
copy x = (x,x)


--data

data Answer = Yes | No | Unknown deriving Show

answers :: [Answer]
answers = [Yes, No, Unknown]

flips :: Answer -> Answer
flips Yes = No
flips No = Yes
flips Unknown = Unknown


--data con parametros

data Shape = Circle Float | Rect Float Float deriving Show

square :: Float -> Shape
square n = Rect n n 

area :: Shape -> Float
area (Circle r) = pi * r ^ 2
area (Rect x y) = x * y 

-- data con parametros de tipos y constructor de tipo

{-data Maybes a = Nothing | Just a deriving Show

safediv :: Int -> Int -> Maybes Int
safediv _ 0 = Nothing
safediv m n = Just (m `div` n)
-}


--data recursiva

data Nat = Zero | Succ Nat deriving (Show)

sumaNat :: Nat -> Nat -> Nat
sumaNat Zero n = n
sumaNat (Succ m) n = Succ (sumaNat m n)

nat2int:: Nat -> Int
nat2int Zero = 0
nat2int (Succ n) = 1 + nat2int n 

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat (n-1))


multiNat :: Nat -> Nat -> Nat
multiNat Zero _ = Zero
multiNat (Succ m) n = sumaNat n (multiNat m n)


expoNat :: Nat -> Nat -> Nat
expoNat _ Zero = Succ Zero
expoNat base (Succ exp) = multiNat base (expoNat base exp)



-- data recursivas y con parametros

data List a = Nil | Cons a (List a) deriving (Show)


to:: List a -> [a]
to Nil = []
to (Cons x xs) = x : (to xs)


from :: [a] -> List a
from [] = Nil
from (x:xs) = Cons x (from xs)


--Sintaxis de registro (Record)

data Person = Person {firstName::String, lastName::String, age::Int, height::Float, phoneNumber::String, flavor::String} deriving (Show)


--Ecpresiones case

esCero :: Nat -> Bool
esCero n = case n of 
                Zero -> True
                _    -> False




-- Clase 3 2/05 BST RBT y LEFTIST HEAPS


-- BST 

data Bin a = Hoja | Nodo (Bin a) a (Bin a) deriving (Show)

member':: Eq a => a -> Bin a -> Bool
member' a Hoja = False
member' a (Nodo l b r) = a == b || member' a l || member' a r 


inOrder:: Bin a -> [a]
inOrder Hoja = []
inOrder (Nodo l a r) = inOrder l ++ [a] ++ inOrder r


member:: Ord a => a -> Bin a -> Bool
member a Hoja = False
member a (Nodo l b r) | a == b = True
                      | a <  b = member a l 
                      | a >  b = member a r


minimum :: Bin a -> a 
minimum (Nodo Hoja a r) = a 
minimum (Nodo l a r) = minimum l 


maximum :: Bin a -> a 
maximum (Nodo l a Hoja) = a 
maximum (Nodo l a r) = maximum r 


insert:: Ord a => a -> Bin a -> Bin a
insert a Hoja = Nodo Hoja a Hoja
insert a (Nodo l b r) | a <= b = Nodo (insert a l) b r 
                      | otherwise = Nodo l b (insert a r)



delete:: Ord a -> a -> Bin a -> Bin a
delete _ Hoja = Hoja
delete z (Nodo l b r) | z <  b = Nodo (delete z l) b r 
delete z (Nodo l b r) | z >  b = Nodo l b (delete z r) 
delete z (Nodo l b r) | otherwise = case (l,r) of
                                    (Hoja, Hoja) -> Hoja 
                                    (Hoja, _) -> r 
                                    (_, Hoja) -> l 
                                    _ -> Nodo minValue l (delete minValue r)
                                          where minValue = minimum r
                                



--RBT 

data Color = R | B 
data RBT a = E | T Color (RBT a) a (RBT a)


memberRBT:: Ord a => a -> RBT a -> Bool
memberRBT a E = False
memberRBT a (T _ l b r) | a == b = True
                        | a <  b = memberRBT a l 
                        | a >  b = memberRBT a r 


insertRBT :: Ord a => a -> RBT a -> RBT a 
insertRBT x t = makeBlack(ins x t)
              where ins x E = T R E x E 
                    ins x (T c l y r) | x < y = balance c (ins x l) y r 
                                      | x > y = balance c l y (ins x r)
                                      | otherwise = T c l y r 



makeBlack E = E
makeBlack (T _ l x r) = T B l x r 


balance:: Color -> RBT a -> a -> RBT a -> RBT a 
balance B (T R (T R a x b) y c) z d = T R (T B a x b) y (T B c z d)
balance B (T R a x (T R b y c)) z d = T R (T B a x b) y (T B c z d)
balance B a x (T R (T R b y c) z d) = T R (T B a x b) y (T B c z d)
balance B a x (T R b y (T R c z d)) = T R (T B a x b) y (T B c z d)
balance c l a r = T c l a r 




--Leftist Heaps

type Rank = Int
data Heap a = E | N Rank a (Heap a) (Heap a)

merge :: Ord a -> Heap a -> Heap a -> Heap a
merge h1 E = h1
merge E h2 = h2
merge h1 @(N x a1 b1 ) h2 @(N y a2 b2 ) = if x 6 y then makeH x a1 (merge b1 h2 )
                                                   else makeH y a2 (merge h1 b2 )


rank :: Heap a -> Rank
rank E = 0
rank (N r ) = r


makeH x a b = if rank a > rank b then N (rank b + 1) x a b
                                 else N (rank a + 1) x b a


insert :: Ord a -> a -> Heap a -> Heap a
insert x h = merge (N 1 x E E) h
findMin :: Ord a ⇒ Heap a → a
findMin (N x a b) = x
deleteMin :: Ord a ⇒ Heap a → Heap a
deleteMin (N x a b) = merge a b