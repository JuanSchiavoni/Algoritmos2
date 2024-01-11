{- Dado el siguiente tipo de dato que representa los numeros naturales:
data Nat = Cero | Succ Nat
a) ¿Que tipo tiene Succ? tipo c
b) Definir la funcion int2Nat :: Int -> Nat que dado un entero retorne su representacin en Nat
Ejemplo int2Nat 4 = Succ (Succ (Succ (Succ Cero)))
c) Definir la funcion suma :: Nat -> Nat -> Nat
NO convertir los Nat a enteros para poder sumarlos.
d) Definir la funcion nat2Int :: Nat -> Int que dado un Nat retorne a que entero representa -}

data Nat = Cero | Succ Nat deriving Show
 
int2Nat :: Int -> Nat
int2Nat 0 = Cero
int2Nat x | x < 0 = error "Num natural no puede ser negativo"
          | otherwise = Succ (int2Nat (x-1))

suma :: Nat -> Nat -> Nat
suma Cero n = n
suma (Succ m) n = Succ (suma m n) --TODO repasar

nat2Int :: Nat -> Int
nat2Int Cero = 0
nat2Int (Succ m) = 1 + nat2Int m

{-2. Dado el tipo de datos de arboles binarios:
    data Arb = E | H Int | N Arb Arb
y el tipo de datos de comandos, para navegar el arbol:
    data Cmd = L | R
a) ¿Que tipo tiene N? arb?
b) Definir la funcion parcial selec::[Cmd] → Arb → Arb, que selecciona el subarbol correspondiente. 
    La funcion selec es parcial, ya que no esta definida para listas de comandos erroneas (como por ejemplo, ir a la izquierda 
    cuando nos encontramos en una hoja).
c) Definir una funcion enum :: Arb → [[Cmd]] que devuelva todas las secuencias de comandos validas para ir desde la raız 
hasta una hoja. -}

data Arb = E | H Int | N Arb Arb deriving Show

data Cmd = L | R deriving (Show, Eq)

selec::[Cmd] -> Arb -> Arb
selec _ E = error "Se encuentra en una hoja"
selec _ (H x)= H x
selec [] (N l r) = N l r
selec (x:xs) (N l r) | x == L = selec xs l
                     | otherwise = selec xs r
-- ejemplo de uso: selec [R,R,L] (N (N (H 2) E) (N E (N (H 2) E)))


enum :: Arb -> [[Cmd]]
enum E = [] -- Caso base: árbol vacío, no hay secuencias válidas
enum (H _) = [[]] -- Caso base: hoja, devuelve una secuencia vacía
enum (N l r) = map (L:) (enum l) ++ map (R:) (enum r)

