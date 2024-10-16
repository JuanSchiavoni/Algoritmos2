type Name = String
data Estado a = Inicial | Define Name a (Estado a)

update :: Name -> a -> Estado a -> Estado a
update name a Inicial = Define name a (Inicial)
update name a (Define n val est) | name == n = Define n a est 
                                 | otherwise = Define n val (update name a est)


lookfor:: Name -> Estado a -> Maybe a
lookfor name Inicial = Nothing
lookfor name (Define n val est) | name == n = Just val
                                | otherwise = lookfor name est

free:: Name -> Estado a -> Estado a 
free name Inicial = Inicial
free name (Define n val est) | name == n = est
                             | otherwise = Define n val (free name est)