{-
Las listas finitas pueden especificarse como un TAD con los constructores:
    nil: Construye una lista vacıa.
    cons: Agrega un elemento a la lista.
y las siguientes operaciones:
    head: Devuelve el primer elemento de la lista.
    tail: Devuelve todos los elementos de la lista menos el primero.


a) Dar una especificacion algebraica del TAD listas finitas

tad List (A : Eq Set) where
    import Bool
    nil   : List A
    cons  : A -> List A -> List A
    head  : List A -> A
    tail  : List A -> List A
    isNil : List A -> Bool
    inL   : List A -> A -> Bool
    elim  : List A -> A -> List A

----------------------------------------
isNil nil                 = True
isNil (cons x y)          = False
head (cons x xs)          = x
tail (cons x nil)         = nil
tail (cons x xs)          = xs
inL nil x                 = False
inL (cons y xs) x         = if_then_else (x==y) True (inL ls x)
elim nil x                = nil
elim (cons y xs) x        = if_then_else (x==y) (elim xs x) (cons y (elim xs x))


b) Asumiendo que A es un tipo con igualdad, especificar una funcion inL : List A → A → Bool
tal que inL ls x = true si y solo si x es un elemento de ls.

c) Especificar una funcion que elimina todas las ocurrencias de un elemento dado.
-}



{-
2. Dado el TAD pilas, con las siguientes operaciones:
     empty: Construye una pila inicialmente vac´ıa.
     push: Agrega un elemento al tope de la pila.
     isEmpty: Devuelve verdadero si su argumento es una pila vac´ıa, falso en caso contrario.
     top: Devuelve el elemento que se encuentra al tope de la pila.
     pop: Saca el elemento que se encuentra al tope de la pila.
    Dar una especificaci´on algebraica del TAD pilas

    tad Stack (A : Set) where
        empty   : Stack A
        push    : A -> Stack A -> Stack A
        isEmpty : Stack A -> Bool
        top     : Stack A -> A
        pop     : Stack A -> Stack A
-----------------------------------------------
        isEmpty empty           = True           
        isEmpty (push x ys)     = False
        top empty               = error
        top (push x ys)         = x
        pop empty               = empty
        pop (push x ys)         = ys


-}


{-
3. Asumiendo que A es un tipo con igualdad, completar la siguiente definici´on del TAD conjunto:
tad Conjunto (A : Set) where
    import Bool
    vacio : Conjunto A
    insertar : A → Conjunto A → Conjunto A
    borrar : A → Conjunto A → Conjunto A
    esVacio : Conjunto A → Bool
    union : Conjunto A → Conjunto A → Conjunto A
    interseccion : Conjunto A → Conjunto A → Conjunto A
    resta : Conjunto A → Conjunto A → Conjunto A
    elem : A -> Conjunto A -> Bool
    ---------------------------------------------------
    x = y ⇒ insertar y (insertar x c) = insertar x c
    x !≡ y ⇒ insertar x (insertar y c) = insertar y (insertar x c)
    esVacio vacio                  = True
    esVacio (insertar x c)         = False
    borrar x vacio                 = vacio
    borrar x (insertar y c)        = if_then_else (x==y) c (insertar y (borrar x c))
    union vacio vacio              = vacio
    union vacio c                  = c
    union c vacio                  = c
    union (insertar x c) s         = insertar x (union c s) 
    elem y vacio                   = False
    elem y (insertar x c)          = if_then_else (y==x) True (elem y c)
    interseccion vacio vacio       = vacio
    interseccion vacio c           = vacio
    interseccion c vacio           = vacio
    interseccion (insertar x c) s  = if_then_else (elem x s) (insert x (interseccion c s)) (interseccion c s)


    ¿Que pasarıa si se agregase una funcion choose : Conjunto A → A, tal que choose (insertar x c) = x ?

-}


{-
4.
El TAD priority queue es una cola en la cual cada elemento tiene asociado un valor que es su
prioridad (a dos elementos distintos le corresponden prioridades distintas). Los valores que definen
la prioridad de los elementos pertenecen a un conjunto ordenado. Las siguientes son las operaciones
soportadas por este TAD:
     vacia: Construye una priority queue vac´ıa.
     poner : Agrega un elemento a una priority queue con una prioridad dada.
     primero: Devuelve el elemento con mayor prioridad de una priority queue.
     sacar : Elimina de una priority queue el elemento con mayor prioridad.
     esVacia: Determina si una priority queue es vac´ıa.
     union: Une dos priority queues.
Dar una especificaci´on algebraica del TAD priority queue

tad PriorityQueue (A: Set, P: OrdSet)
    vacia : PriorityQueue A P
    poner : A -> P -> PriorityQueue A P -> PriorityQueue A P
    primero : PriorityQueue A P -> A
    sacar : PriorityQueue A P -> PriorityQueue A P
    esVacia : PriorityQueue A P -> Bool
    union : PriorityQueue A P -> PriorityQueue A P -> PriorityQueue A P
---------------------------------------------------------------------------------
    esVacia vacia                       = True
    esVacia (poner x y z)               = False
    primero vacia                       = error
    primero (poner x y vacia)           = x
    primero (poner x y (poner i r s))   = if_then_else (y>r) (x) (primero(poner i r s))
    sacar vacia                         = error
    sacar (poner x y vacia)             = vacia
    sacar (poner x y (poner i r s))     = if_then_else (y>r) (poner i r s) (poner x y (sacar(poner i r s)))
    union vacia vacia                   = vacia
    union vacia c                       = c
    union c vacia                       = c
    union (poner x p c) z               = poner (x p union(c z)) 

-}

{-
Demostrar que (uncurry zip) ◦ unzip = id, siendo:
zip :: [a] → [b] → [(a, b)]
zip [] ys = []
zip (x:xs) [] = []
zip (x:xs) (y:ys) = (x, y) : zip xs ys

unzip :: [(a,b)] → ([a], [b])
unzip [] = ([], [])
unzip ((x, y) : ps) = (x:xs, y:ys)
                    where (xs, ys) = unzip ps

curry f a b = f (a,b)
uncurry f (a,b) = f a b

Hay que demostrar: 
(uncurry zip) ◦ unzip = id

(f.g) x = f(g x)
id x = x

sea l :: [(a,b)]

caso l = []

((uncurry zip) . unzip) []
                        <def (.)>
uncurry zip(unzip []) 
                        <def unzip.1>
uncurry zip ([], [])
                        <def uncurry>
zip [] []         
                        <def zip.1>
[]
                        <def id>
id []

((uncurry zip) . unzip) [] = id [] luego por extensionalidad
 (uncurry zip) . unzip = id


caso l = r:rs, r = (x,y)
HI ((uncurry zip) . unzip) rs = id rs

((uncurry zip) . unzip) (r:rs) = id (r:rs)    Hay que llegar a esto

((uncurry zip) . unzip) (r:rs)
                                <def (.)>
uncurry zip (unzip ((x,y):rs))
                                <def (unzip.2)>
uncurry zip ((x:xs,y:ys) where (xs,ys) = unzip xs)
                                <def reescritura xs = fst (unzip rs)
                                                 ys = snd (unzip rs)>
uncurry zip (x:(fst (unzip rs)), y:(snd (unzip rs)))
                                <def uncurry>
zip (x:(fst (unzip rs))) (y:(snd (unzip rs)))
                                <def zip.3>
(x,y) : zip (fst (unzip rs)) (snd (unzip rs))
                                <def uncurry>
(x,y) : uncurry zip ((fst (unzip rs)),(snd (unzip rs)))
                                <def reescritura xs = fst (unzip rs)
                                                 ys = snd (unzip rs)>
(x,y) : uncurry zip (unzip rs)
                                <def (.)>
(x,y) : (uncurry zip . unzip) rs
                                <def HI>
(x,y) : id rs
                                <def id>
(x,y) : rs
                                <def id>
id ((x,y) :rs)

((uncurry zip) . unzip) (r:rs) = id (r:rs) por extensionalidad

(uncurry zip) . unzip = id

-}


{-
6. Demostrar que sum xs <= length xs ∗ maxl xs, sabiendo que xs es una lista de numeros naturales
y que maxl y sum se definen:
maxl [ ] = 0 
maxl (x : xs) = x ‘max‘ maxl xs
sum [ ] = 0
sum (x : xs) = x + sum xs

queremos demostrar que sum xs <= length xs * maxl xs

Caso base xs = []

sum [] = 0
length [] = 0
maxl [] = 0

Entonces, se demuestra que sum 0 <= length 0 * maxl 0



caso xs = (y:ys)
length ys = n
length xs = n+1

sum (y:ys) <= length (y:ys) * maxl (y:ys)   Hay que demostrar esto
                                    <def sum.2, length, maxl.2>
y + sum ys <= (n+1) * (y 'max' maxl ys)

Existen dos casos:
y + sum ys <= (n+1) * y
y + sum ys <= (n+1) * maxl ys


-}