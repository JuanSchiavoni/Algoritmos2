type Tam = Int
data Vector a = E | N Tam (Vector a) (Maybe a) (Vector a) deriving Show

center :: Int -> Int
center = (`div` 2)

ini :: Int -> Vector a
ini 0 = E
ini n = N n (ini (center n)) Nothing (ini (n - center n - 1))

ins :: Int -> a -> Vector a -> Vector a
ins _ _ E = E
ins pos e v@(N n l c r) | pos < 0 || pos >= n = v 
                        | pos == center n = N n l (Just e) r
                        | pos < center n = N n (ins pos e l) c r
                        | otherwise = N n l c (ins (pos-center n-1) e r)

view :: Int -> Vector a -> Maybe a
view _ E = Nothing
view pos (N n l c r) | pos < 0 || pos >= n = Nothing 
                     | pos == center n = c
                     | pos < center n = view pos l
                     | otherwise = view (pos-center n-1) r

reverse' :: Vector a -> Vector a
reverse' E = E
reverse' (N n l c r) = N n (reverse' r) c (reverse' l)