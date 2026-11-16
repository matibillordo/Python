longitud :: [Integer] -> Integer
longitud [] = 0
longitud (x:xs) = 1 + longitud(xs)

ultimo :: [Integer] -> Integer
ultimo [x] = x
ultimo (x:xs) = ultimo xs

ultimaLetra :: [t] -> t -- o tambien puede ser String -> Char
ultimaLetra [x] = x
ultimaLetra (x:xs) = ultimaLetra xs

principio :: [Integer] -> [Integer]
principio [x] = []
principio (x:xs) = x : principio xs

--comentada para seguir practicando cómo hacerla, ya que es una funcion util, como la de pertenece
--reverso :: [t] -> [t]
--reverso [] = []
--reverso(x:xs) = reverso(xs) ++ [x]

pertenece :: Eq t => t -> [t] -> Bool
pertenece t [] = False
pertenece t (x:xs)
    | x == t = True
    | otherwise = pertenece t (xs) 

todosIguales :: Eq t => [t] -> Bool
todosIguales [x] = True
todosIguales (x:y:xs)
    | x == y = todosIguales(y:xs)
    | otherwise = False
    

todosDistintos :: Eq t => [t] -> Bool
todosDistintos [x] = True
todosDistintos (x:y:xs)
    | not (pertenece x (y:xs)) = todosDistintos(y:xs)
    | otherwise = False

hayRepetidos :: Eq t => [t] -> Bool
hayRepetidos [x] = False
hayRepetidos (x:y:xs)
    | pertenece x (y:xs) = True
    | otherwise = hayRepetidos (y:xs)

quitar :: Eq t => t-> [t]-> [t]
quitar m [] = []
quitar m (x:xs)
    | m == x = xs
    | otherwise = x : quitar m xs

quitarTodas :: Eq t => t-> [t]-> [t]
quitarTodas m [] = []
quitarTodas m (x:xs)
    | m == x = quitarTodas m xs
    | otherwise = x : quitarTodas m xs

eliminarRepetidos :: Eq t => [t]-> [t]
eliminarRepetidos [] = []
eliminarRepetidos (x:xs)
    | pertenece x xs = eliminarRepetidos xs
    | otherwise = x : eliminarRepetidos xs

--pertenece2 :: Eq t => t -> [t] -> Bool
--pertenece2 t [] = False
--pertenece2 t (x:xs)
--    | t == x = True
--    | otherwise = pertenece2 t xs

incluida :: Eq t => [t] -> [t] -> Bool
incluida [] _ = True
incluida (x:xs) y
    | pertenece x y = incluida xs y
    | otherwise = False

mismosElementos :: Eq t => [t] -> [t] -> Bool
mismosElementos [] [] = True
mismosElementos x y 
    | incluida x y && incluida y x = True
    | otherwise = False

listasIguales :: Eq t => [t] -> [t] -> Bool
listasIguales [] [] = True
listasIguales (x:xs) (y:ys)
    | x == y = listasIguales xs ys
    | otherwise = False

rever :: Eq t => [t] -> [t]
rever [] = []
rever (x:xs) = rever xs ++ [x] 

capicua :: Eq t => [t] -> Bool
capicua [] = True
capicua lista
    | (listasIguales (rever lista) lista) = True -- evalúo la funcion revertida y veo si es igual a la original
    | otherwise = False

--3 
sumatoria :: [Integer] -> Integer
sumatoria [] = 0
sumatoria (x:xs) = x + sumatoria xs

maximo :: [Integer] -> Integer
maximo [x] = x
maximo (x:y:xs)
    | x >= y = maximo (x:xs)
    | otherwise = maximo (y:xs)

sumarN :: Integer-> [Integer]-> [Integer]
sumarN n [] = []
sumarN n (x:xs) = (n+x) : sumarN n xs

sumarPrimero :: [Integer]-> [Integer]
sumarPrimero [] = []
sumarPrimero (x:xs) = sumarN x (x:xs)

devuelveUltimoElemento :: Eq t => [t] -> t
devuelveUltimoElemento [x] = x
devuelveUltimoElemento (x:xs) = devuelveUltimoElemento xs

sumarUltimo :: [Integer] -> [Integer]
sumarUltimo []=[]
sumarUltimo x = sumarN (devuelveUltimoElemento x) x 

numeroEsPar :: Integer -> Bool
numeroEsPar x
    | mod x 2 == 0 = True
    | otherwise = False


pares :: [Integer]-> [Integer]
pares [] = []
pares (x:xs)
    | numeroEsPar x = x : pares xs
    | otherwise = pares xs 

multiploDeN :: Integer -> Integer -> Bool
multiploDeN numero n
    | mod numero n == 0 = True
    | otherwise = False

listaConMultiplosDeN :: Integer-> [Integer]-> [Integer]
listaConMultiplosDeN n [] = []
listaConMultiplosDeN n (x:xs)
    | multiploDeN x n = x : listaConMultiplosDeN n xs
    | otherwise = listaConMultiplosDeN n xs

buscaMax :: [Integer] -> Integer
buscaMax [x] = x
buscaMax (x:y:xs)
    | x >= y = buscaMax (x:xs)
    | otherwise = buscaMax (y:xs)

quitaTodosLosN :: Integer -> [Integer] -> [Integer]
quitaTodosLosN n [] = []
quitaTodosLosN n (x:xs)
    | n == x = quitaTodosLosN n xs
    | otherwise = x : quitaTodosLosN n xs

quitaPrimeraAparicionDeN :: Integer -> [Integer] -> [Integer]
quitaPrimeraAparicionDeN n [] = []
quitaPrimeraAparicionDeN n (x:xs)
    | n == x = xs
    | otherwise = x: quitaPrimeraAparicionDeN n xs

ordenaMayorAMenor :: [Integer] -> [Integer]
ordenaMayorAMenor [] = []
ordenaMayorAMenor [x] = [x]
ordenaMayorAMenor (x:xs) = (buscaMax (x:xs)) : ordenaMayorAMenor(quitaPrimeraAparicionDeN (buscaMax (x:xs)) (x:xs))

--4
sacarBlancosRepetidos :: [Char] -> [Char]
sacarBlancosRepetidos [] = []
sacarBlancosRepetidos [x] = [x]
sacarBlancosRepetidos (x:y:xs) 
    | (x ==  ' ') && (y == ' ') = sacarBlancosRepetidos (x:xs)
    | otherwise = x : sacarBlancosRepetidos (y:xs)

contarPalabras :: [Char]-> Integer
contarPalabras [x] = 1
contarPalabras (x:xs)
    | x == ' ' = 1 + contarPalabras xs
    | otherwise = contarPalabras xs

palabras :: [Char]-> [[Char]]
palabras x = [x]