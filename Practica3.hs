{-1. Sea Fn la sucesion de Fibonacci:
F1 = 1
F2 = 1
Fn+2 = Fn+1 + Fn

Desarrollar f´ormulas para las siguientes sumas:
suponiendo q n = 3
E(F2i-1) = F(7) - F(5)
E(F2i-1) = F(2*n+1) - F(2*n-1)

i        |  1  |  2  |  3  |  4   |  5   |  6   |
2i-1     |  1  |  3  |  5  |  7   |  9   |  11  |
F2i-1    |  1  |  2  |  5  |  13  |  34  |  89  |
E(F2i-1) |  1  |  3  |  8  |  21  |  55  |  144 |

i        |  1  |  2  |  3  |  4   |  5   |  6   |
2i       |  2  |  4  |  6  |  8   |  10  |  12  |
F2i      |  1  |  3  |  8  |  21  |  55  |  144 |
E(F2i)   |  1  |  4  |  12 |  33  |  88  |  232 |

suponiendo n = 3
E(F2i) = F(8) - F(6) - 1
E(F2i) = F(2*n + 2) - F(2*n) - 1 
-}

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n - 2)

sumfib :: Int -> Int
sumfib 1 = fib 1
sumfib n = fib(2*n-1) + sumfib(n-1)

sumfib2 :: Int -> Int
sumfib2 1 = fib 1
sumfib2 n = fib (2*n) + sumfib2(n-1)


-- 2. Encontrar una forma cerrada para la siguiente sumatoria:
{-
 n           n         n            n       n               n
 E (a+bi) =  E  a   +  E   bi  = a  E  1 +  E i  =  a*n  +  E i  = a*n + n * (n+1)  
i=0         i=0       i=0          i=0     i=0             i=1              2
-}


-- 3. ¿Cu´ales de los siguientes enunciados son verdaderos? Probar las respuestas.
{-
Definicion (O)
Sean f, g : N → R. Decimos que f tiene orden de crecimiento O(g) (y
escribimos f ∈ O(g)), si existen constantes c ∈ R+, n0 ∈ N, tales que:
∀ n >= n0 · 0 <= f(n) <= c g(n)

--1. n^2 ∈ O(n^3)
f(n) = n^2
g(n) = n^3

0 <= f(n) <= c g(n)
0 <= n^2  <= c n^3
0 <= 1 <= c n

n_0 = 1
c = 1
-}

{-
Definicion (Ω)
Sean f, g : N → R. Decimos que f tiene orden de crecimiento Ω(g) (y
escribimos f ∈ Ω(g)), si existen constantes c ∈ R+, n0 ∈ N, tales que:
∀n >= n0 · 0 <= c g(n) <= f(n)

2. n^2 ∈ Ω(n^3)
f(n) = n^2
g(n) = n^3

0 <= c g(n) <= f(n)
0 <= c n^3 <= n^2
0 <= c n <= 1

n_0 = 
c = 
FALSO!!!! pues n ∈ N y no esta acodato superiormente
-}

{-
Definicion (Θ)
Sean f, g : N → R. Decimos que f tiene orden de crecimiento Θ(g) (y
escribimos f ∈ Θ(g)), si existen constantes c1c2 ∈ R+, n0 ∈ N, tales que:
∀ n >= n0 · 0 <= c1 g(n) <= f(n) <= c2 g(n)

3. 2^n ∈ Θ(2^n+1)
f(n) = 2^n
g(n) = 2^n+1

0 <= c1 g(n)  <= f(n)  <= c2 g(n)
0 <= c1 2^n+1 <= 2^n   <= c2 2^n+1
                 1     <= 2
                 1 2^n <= 2 2^n
                 2^n   <= 1^n+1, c2 = 1
     c1 2^n+1 <= 2^n
     c1 2 2^n <= 2^n
     1/2 2 2^n <= 2^n, c1 = 1/2
-}

{-
 4. n! ∈ Θ((n + 1)!)

 0 <= c1 (n + 1)! <= n! <= c2 (n + 1)!
 0 <= c1 (n)! * (n+1) <= n! <= c2 (n)! * (n+1)
 0 <= c1 * (n+1) <= 1 <= c2 * (n+1)

 1 <= c2 (n+1) ----> c2 n + c2 ---->  1 <= c2 n + c2 ---> 1 - c2 <= c2 n ---> C2=1  entonces n > 0 --> n_0=1

FALSO!!!! pues n ∈ N y no esta acodato superiormente

n = 3
n! 3!
1 * 2 * 3

(n+1)! 4!
1 * 2 * 3 * 4

(n)! * (n+1)
3!          * 4
1 * 2 * 3   * 4

-}


--4. Demostrar que f ∈ Θ(g) si y solo si f ∈ O(g) y g ∈ O(f)
{-
Definicion (O)
Sean f, g : N → R. Decimos que f tiene orden de crecimiento O(g) (y
escribimos f ∈ O(g)), si existen constantes c ∈ R+, n0 ∈ N, tales que:
∀ n >= n0 · 0 <= f(n) <= c g(n)


Definicion (Θ)
Sean f, g : N → R. Decimos que f tiene orden de crecimiento Θ(g) (y
escribimos f ∈ Θ(g)), si existen constantes c1c2 ∈ R+, n0 ∈ N, tales que:
∀ n >= n0 · 0 <= c1 g(n) <= f(n) <= c2 g(n)

Existe n0 ∈ N, c1, c2 ∈ R+  tal que  0 <= c1 g(n) <= f(n) <= c2 g(n)     ∀ n >= n0

                                   0 <= f(n) <= c2 g(n)       ∀ n >= n0 y c2 ∈ R+
                                        f ∈ O(g
                              y
                                   0 <= c1 g(n) <= f(n)       ∀ n >= n0 y c1 ∈ R+ 
                                        f ∈ Ω(g)
                                   0 <= g(n) <= 1/c1 f(n)     ∀ n >= n0 y c = 1/c1 ∈ R+
                                        g ∈ O(f)

Entonces tenemos:
                    0 <= g(n) <= 1/c1 f(n)
                    0 <= f(n) <= c2 g(n)

suponiendo que f ∈ O(g) y g ∈ O(f) tenemos c1,c2 > 0 y enteros n1,n2 ∈ N tales que
     f(n) <= c1 g(n)     ∀ n >= n1
     g(n) <= c2 f(n)     ∀ n >= n2

queremos demostrar que existe n0 y c3 c4 tales que
     0 <= c3 g(n) <= f(n) <= c4 g(n) ∀ n >= n0

 Dado f(𝑛) ≤ 𝑐1 g(n), podemos tomar 𝑐4 = 𝑐1
 Dado g(n) <= c2 f(n) podemos hacer f(n) >= 1/c2 g(n)  y tomamos 1/c2 = c3

 entonces 
     1/c2 g(n) <= f(n) <= c1 g(n)

esto significa que
         0 <= c3 g(n) <= f(n) <= c4 g(n)  donde c3 = 1/c2 y c4 = c1 

tenemos que f ∈ Θ(g)                              
-}

{-
5. Sean f, g : N → R asintoticamente no negativas y h(n) = f(n) + g(n), demostrar que
                                                                    h(n) ∈ Θ(max(f(n), g(n)))
h(n) = f(n) + g(n)
max(f(n), g(n))
h(n) ∈ Θ(max(f(n), g(n)))

∀ n >= n0 · 0 <= c1 g(n) <= f(n) <= c2 g(n)

0 <= c1 max(f(n), g(n)) <= h(n) <= c2 max(f(n), g(n))
0 <= c1 max(f(n), g(n)) <= f(n) + g(n) <= c2 max(f(n), g(n))

-}

{-
6. Dadas f, g : N → R, demostrar las siguientes propiedades de las notaciones asint´oticas:

1. O y Ω son transitivas 

O transitiva
Definicion O:  0 <= f(n) <= c g(n)
                         aRb        bRc            ==>      aRc
Es transitiva si: f(n) = O(g(n)) y g(n) = O (h(n)) ==> f(n) = O(h(n))

     0 <= f(n) <= c g(n)
     Encontrar c1 y n1 que f(n) <= c1 g(n) para todo n >= n1

     0 <= g(n) <= c h(n)
     Encontrar c2 y n2 que g(n) <= c2 h(n) para todo n >= n2

     0 <= f(n) = O(h(n))
     Encontrar c3 y n3 que f(n) <= c3 h(n) para todo n >= n3

     f(n) <= c1 g(n)   y    g(n) <= c2 h(n)

     f(n) <= c1 * c2 * h(n)
     
     Si imaginamos que c3 = c1 * c2
     f(n) <= c3 * h(n) para todo n >= max(n1, n2)


Ω transitiva
Definicion Ω:  0 <= c g(n) <= f(n)
Es transitiva si: f(n) = Ω(g(n)) y g(n) = Ω (h(n)) ==> f(n) = Ω(h(n))

     0 <= c * g(n) <= f(n)
     Encontrar c1 y n1 que c1 * g(n) <= f(n) para n >= n1

     0 <= c * h(n) <= g(n)
     Encontrar c2 y n2 que c2 * h(n) <= g(n) para todo n >= n2

     Entonces para demostrar que f(n) = Ω(h(n))
     Tienen que existir c3 y n3 para que c3 * h(n) <= f(n)

     Teniendo
     c1 * g(n) <= f(n)   y   c2 * h(n) <= g(n)

     c1 * c2 * h(n) <= f(n)

     Tomando c1 * c2 = c3, quedaria:
     0 <= c3 * h(n) <= f(n) para todo n <= max (n1, n2)



2. f asintoticamente no negativa ⇒ f(n) ∈ Θ(f(n))

Definicion (Θ)
0 <= c1 g(n) <= f(n) <= c2 g(n)

Una funcion f : N → R es asintoticamente no negativa cuando
    
    ∃ N ∈ N   tal que  ∀ n ≥ N    f(n) ≥ 0 

    0 <= c1 f(n) <= f(n) <= c2 f(n)

    Determinando que c1 y c2 = 1, entonces:
    0 <= 1 f(n) <= f(n) <= 1 f(n)



3. f(n) ∈ O(g(n)) ⇔ g(n) ∈ Ω(f(n))

   0 <= f(n) <= c g(n)   ⇔   0 <= c f(n) <= g(n)

   1era parte----

    f(n) <= c g(n) ⇒   c f(n) <= g(n)

    0 <= f(n) <= c g(n) para todo n >= n0

    si tomamos a k = 1/c tenemos 
    k f(n) <= g(n) 

    por lo tanto g(n) ∈ Ω(f(n)) 

    2da parte----

    c f(n) <= g(n) ⇒ f(n) <= c g(n)

    tomando a k = 1/c tenemos nuevamente 

    f(n) <= k g(n) por lo tanto 
    
    f(n) ∈ O(g(n))

-}


{-
8. Dadas las siguientes definiciones en Haskell, calcular el Work de cada una de ellas.
num_1 0 = 1
num_1 n = sqrt(num_1 (n − 1)) + n

Wnum_1(0) = k0
Wnum_1(1) = Wsqrt + Wnum_1 (n-1) + k1  si n > 0

sqrt :: Floating a => a -> a
sqrt x = x ** 0.5

Wsqrt(n) = k2

Wnum_1(n) = Wsqrt(n) + Wnum_1 (n-1) + k1 si n > 0
donde n es el valor ingresado



num_2 0 = 1
num_2 n = 2 ∗ (num_2 (div n 2)) + 1

Wnum_2 (0) = k0
Wnum_2 (n) = Wmult + (Wnum_2(div n 2)) + k1  donde n > 0
Wmult = k2

Wnum_2 (4) = 2 * Wnum_2(div 2 2) + k1
Wnum_2 (2) = 2 * Wnum_2(div 1 2) + k1
Wnum_2 (1) = 2 * Wnum_2(div 0 2) + k1
wnum_2 (0) = k0
             
-}

