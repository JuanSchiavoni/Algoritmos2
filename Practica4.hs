{-
1. Muestre utilizando el metodo de sustitucion que la solucion de la recurrencia
T(n) = T(|n/2˩) + 1       es piso

es O(lg n)

Definicion O
Sean f, g : N → R
c ∈ R+, n0 ∈ N
0 <= f(n) <= c g(n)

T(n) <= c lg n

Suponemos:
T(|n/2˩) <= c lg |n/2˩

T(|n/2˩) + 1 <= c lg |n/2˩ + 1 <= c (lg n/2) + 1 <= c (lg n - lg 2 ) + 1 <= c (lg n - 1) + 1 <= (c lg n) - c + 1 <= c lg n
c >= 1

Entonces, T(n) <= c lg n    si c >= 1

Buscar casos base:
n=1
T(0) = k0
T(1) = T(|1/2˩) + 1 = k + 1 <= c lg 1   no vale aca

n=2
t(1) = k1
T(2) = T(|2/2˩) + 1 = k1 + 1 <= c lg 2  <= c * 1

Para el caso base:
c=2 y n=2

==========================================================================================================

2. Sean a, b ∈ R+, utilizar el metodo de sustitucion para encontrar cotas asintotica Θ para las
siguientes recurrencias:
        a                    n = 1
T(n) = 
        2T (|n/2˩) + n       n > 1


Definicion (Θ)
Sean f, g : N → R
c1 c2 ∈ R+, n0 ∈ N
0 <= c1 g(n) <= f(n) <= c2 g(n)

==========================================================================================================

3. Utilice un ´arbol de recurrencia para encontrar una cota asint´otica para la recurrencia
T(n) = 4T(n/2) + cn
donde c es una constante. Verifique que la cota encontrada es correcta.


                                cn
                            /    |    |    \
                           /     |    |     \
                       T(n/2)  T(n/2) T(n/2) T(n/2) 
                       
-}