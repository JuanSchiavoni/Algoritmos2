type Tam = Int 
data Vector a = E | N Tam (Vector a) (Maybe a) (Vector a)

ini :: Tam -> Vector a 
ini 0 = E 
ini n = N n (ini (n - 1)) Nothing (ini (n - 1))