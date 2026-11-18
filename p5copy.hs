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

--contarPalabras :: [Char]-> Integer
--contarPalabras [x] = 1
--contarPalabras (x:xs)
--    | x == ' ' = 1 + contarPalabras xs
--    | otherwise = contarPalabras xs

obtienePrimerPalabra :: [Char] -> [Char]
obtienePrimerPalabra [] = []
obtienePrimerPalabra (x:xs)
    | x == ' ' = []
    | otherwise = x : obtienePrimerPalabra xs

sacaPrimerPalabra :: [Char] -> [Char]
sacaPrimerPalabra [] = []
sacaPrimerPalabra (x:xs)
    | x == ' ' = xs 
    | otherwise = sacaPrimerPalabra xs

separa :: [Char] -> [[Char]]
separa []=[]
separa (x:xs) = obtienePrimerPalabra (x:xs) : separa (sacaPrimerPalabra (x:xs))

longitu :: [Char] -> Integer
longitu []= 0
longitu (x:xs) = 1 + longitu xs 


masLargaConListas :: [[Char]] -> [Char]
masLargaConListas []=[]
masLargaConListas [x] = x
masLargaConListas (x:y:xs)
    | longitu x >= longitu y = masLargaConListas (x:xs)
    | otherwise = masLargaConListas (y:xs)

palabraMasLarga :: [Char] -> [Char]
palabraMasLarga lista = masLargaConListas (separa lista)

aplanar :: [[Char]] -> [Char]
aplanar [x] = x
aplanar (x:xs) = x ++ " " ++aplanar xs

insertaNBlancos :: Integer ->[Char] 
insertaNBlancos 0 = []
insertaNBlancos n = insertaNBlancos(n-1) ++ " " 

aplanarConNBLancos :: Integer -> [[Char]] -> [Char]
aplanarConNBLancos _ [x] = x
aplanarConNBLancos n (x:xs) = x ++ insertaNBlancos n ++aplanarConNBLancos n xs

-- Función principal pedida por la especificación
descomponerEnPrimos :: [Integer] -> [[Integer]]
descomponerEnPrimos [] = []
descomponerEnPrimos (x:xs) = descomponerNumero x : descomponerEnPrimos xs

-- Descompone un único número en su lista de factores primos
descomponerNumero :: Integer -> [Integer]
descomponerNumero 1 = []
descomponerNumero n = p : descomponerNumero (div n p)
  where p = menorDivisor n

-- Encuentra el menor divisor mayor a 1 de un número (siempre es primo)
menorDivisor :: Integer -> Integer
menorDivisor n = menorDivisorDesde n 2

menorDivisorDesde :: Integer -> Integer -> Integer
menorDivisorDesde n d
    | mod n d == 0 = d
    | otherwise    = menorDivisorDesde n (d + 1)


type Texto = [Char]
type Nombre = Texto     -- -> = [Char]
type Telefono = Texto   -- -> = [Char]
type Contacto = (Nombre, Telefono)
type ContactosTel = [Contacto]

-- Dado un Contacto (tupla), devuelve el primer elemento
elNombre :: Contacto -> Nombre
elNombre (nombre, _) = nombre

elTelefono :: Contacto -> Telefono
elTelefono (_ , telefono) = telefono

enLosContactos :: Nombre-> ContactosTel-> Bool
enLosContactos _ [] = False
enLosContactos nombreBuscado (x:xs)
    | nombreBuscado == elNombre(x) = True
    | otherwise = enLosContactos nombreBuscado xs

--"Mati" [("Sofi","2001"),("Vale","2401"),("Mati","2021")]

agregarContacto :: Contacto-> ContactosTel-> ContactosTel
agregarContacto contacto [] = contacto : [] -- == [contacto]
agregarContacto contacto (x:xs)
    | elNombre(contacto) == elNombre(x) = contacto : xs
    | otherwise = x : agregarContacto contacto xs

eliminarContacto :: Nombre-> ContactosTel-> ContactosTel
eliminarContacto _ [] = []
eliminarContacto nombre (primerElemento:restoDeLista)
    | nombre == elNombre(primerElemento) = restoDeLista
    | otherwise = primerElemento : eliminarContacto nombre restoDeLista

type Identificacion = Integer
type Ubicacion = Texto
type Estado = (Disponibilidad, Ubicacion)
type Locker = (Identificacion, Estado)
type MapaDeLockers = [Locker]
type Disponibilidad = Bool
-- lockers=[(100,(False,"ZD39I")) , (101,(True,"JAH3I")) , (103,(True,"IQSA9")) , (105,(True,"QOTSA")) , (109,(False,"893JJ")),(110,(False,"99292"))]

laDisponibilidadDelLocker :: Estado -> Bool
laDisponibilidadDelLocker(disponibilidad,_)=disponibilidad

laUbicacionDelLocker :: Estado -> Ubicacion
laUbicacionDelLocker(_,ubicacion)=ubicacion

laIdentificacionDelLocker :: Locker -> Identificacion
laIdentificacionDelLocker(identificacion,_)=identificacion

elEstadoDelLocker :: Locker -> Estado
elEstadoDelLocker(_,estado)=estado


existeElLocker :: Identificacion->MapaDeLockers->Bool
existeElLocker identificacion [] = False
existeElLocker identificacion (primerLocker:restoDeLockers)
    | identificacion == laIdentificacionDelLocker(primerLocker) = True
    | otherwise = existeElLocker identificacion restoDeLockers

ubicacionDelLocker :: Identificacion->MapaDeLockers->Ubicacion
ubicacionDelLocker _ [] = []
ubicacionDelLocker identificacion (primerLocker:restoDeLockers)
    | identificacion == laIdentificacionDelLocker(primerLocker) = laUbicacionDelLocker(elEstadoDelLocker(primerLocker))
    | otherwise = ubicacionDelLocker identificacion restoDeLockers

estaDisponibleElLocker :: Identificacion->MapaDeLockers->Bool
estaDisponibleElLocker identificacion [] = False
estaDisponibleElLocker identificacion (((id),(estado,ubicacion)):restoDeLockers) --desempaqueto sin 
    | identificacion == id = estado
    | otherwise = estaDisponibleElLocker identificacion restoDeLockers

ocuparLocker :: Identificacion->MapaDeLockers->MapaDeLockers
ocuparLocker _ [] = []
ocuparLocker identificacion (((id),(estado,ubicacion)):restoDeLockers)
    | identificacion == id && estado = ((identificacion),(False,ubicacion)) : restoDeLockers
    | otherwise = ((id),(estado,ubicacion)) : ocuparLocker identificacion restoDeLockers

--ocuparLocker :: Identificacion->MapaDeLockers->MapaDeLockers
--ocuparLocker _ [] = []
--ocuparLocker identificacion (locker:restoDeLockers)
--    | identificacion == laIdentificacionDelLocker(locker) && elEstadoDelLocker(locker) == True = [(identificacion),(False,_)] : restoDeLockers
--    | otherwise = locker : ocuparLocker identificacion restoDeLockers

type Matriz = [[Integer]]

sumaFila :: [Integer] -> Integer
sumaFila []= 0
sumaFila (x:xs) = x + sumaFila xs

sumaTotal :: [[Integer]] -> Integer
sumaTotal [] = 0
sumaTotal (x:xs) = (sumaFila x) +  sumaTotal xs

recorreFila :: Integer -> [Integer] -> Integer
recorreFila _ [] = 0
recorreFila numero (x:xs)
    | numero == x = 1 + recorreFila numero xs
    | otherwise = recorreFila numero xs

cantidadDeApariciones :: Integer -> [[Integer]] -> Integer
cantidadDeApariciones _ [] = 0
cantidadDeApariciones numero (x:xs) = (recorreFila numero x) + cantidadDeApariciones numero xs

aparicionesEnFila :: String -> [String] -> Integer
aparicionesEnFila _ [] = 0
aparicionesEnFila palabra (x:xs)
    | palabra == x = 1 + aparicionesEnFila palabra xs
    | otherwise = aparicionesEnFila palabra xs

contarPalabras :: String->[[String]]->Integer
contarPalabras _ [] = 0
contarPalabras palabra (x:xs) = (aparicionesEnFila palabra x) + contarPalabras palabra xs 

filaApariciones :: Eq t => t -> [t] -> Integer
filaApariciones _ [] = 0
filaApariciones t (x:xs)
    | t == x = 1 + filaApariciones t xs
    | otherwise = filaApariciones t xs

cantApar :: Eq t => t -> [[t]] -> Integer
cantApar _ [] = 0
cantApar t (x:xs) = (filaApariciones t x) + cantApar t xs

multiplicarPorEscalarFila ::Integer-> [Integer]->[Integer]
multiplicarPorEscalarFila _ [] = []
multiplicarPorEscalarFila lambda (x:xs) = (lambda * x):multiplicarPorEscalarFila lambda xs 

multiplicarPorEscalar :: Integer-> [[Integer]]-> [[Integer]] 
multiplicarPorEscalar _ [] = []
multiplicarPorEscalar lambda (x:xs) = (multiplicarPorEscalarFila lambda x):multiplicarPorEscalar lambda xs

concatenarFilas :: [String]->String
concatenarFilas [x] = x
concatenarFilas (x:xs) = x ++ " "++  concatenarFilas xs

concatenarFilasFinal :: [[String]]->[String]
concatenarFilasFinal [] = []
concatenarFilasFinal (x:xs) = (concatenarFilas x) :  concatenarFilasFinal xs

iesimoElemento :: Integer-> [a]-> a
iesimoElemento 0 (x:_)  = x
iesimoElemento i (x:xs) = iesimoElemento (i-1) xs

iesimaColumna :: Integer-> [[a]]-> [a]
iesimaColumna _ []    = []
iesimaColumna i (x:xs)= (iesimoElemento i x): iesimaColumna i xs



cantDeFilas :: [[Integer]] -> Integer
cantDeFilas [] = 0
cantDeFilas (x:xs)= 1 + cantDeFilas xs


chequeaFila ::Integer-> Integer->[Integer] -> Bool
chequeaFila _ _ [] = True
chequeaFila posicionActual posUno (x:xs) 
    | posicionActual == posUno && x == 1 = chequeaFila (posicionActual+1) posUno xs
    | posicionActual /= posUno && x == 0 = chequeaFila (posicionActual+1) posUno xs
    | otherwise                          = False

matrizIdentidad :: Integer->[[Integer]] -> Bool
matrizIdentidad _ [] = True
matrizIdentidad fila (x:xs)
    | (chequeaFila 0 fila x) = matrizIdentidad (fila+1) xs
    | otherwise = False

mIdentidad :: [[Integer]] -> Bool
mIdentidad m = matrizIdentidad 0 m

--[[1,0,0][0,1,0][0,0,1]]



iesimoLugar ::Integer->[Integer] -> Integer
iesimoLugar 1 (x:_) = x
iesimoLugar i (x:xs) = iesimoLugar(i-1)xs


cantidadParesColumna :: Integer-> [[Integer]]-> Integer
cantidadParesColumna _ [] = 0
cantidadParesColumna i (x:xs)
    | numeroEsPar(iesimoLugar i x) = 1 + cantidadParesColumna i xs
    | otherwise = cantidadParesColumna i xs

sumarIEnDiagonal :: Integer -> Integer -> Integer -> [Integer] -> [Integer]
sumarIEnDiagonal _ _ _ [] = []
sumarIEnDiagonal n colActual posDiagonal (x:xs)
    | colActual == posDiagonal = (n + x) : xs
    | otherwise                = x : sumarIEnDiagonal n (colActual + 1) posDiagonal xs

sumarEnDiagonalMatriz :: Integer -> Integer -> [[Integer]] -> [[Integer]]
sumarEnDiagonalMatriz _ _ [] = []
sumarEnDiagonalMatriz n filaActual (x:xs) = sumarIEnDiagonal n 0 filaActual x : sumarEnDiagonalMatriz n (filaActual + 1) xs

ejercicio6 :: [[Integer]] -> Integer -> [[Integer]]
ejercicio6 matriz n = sumarEnDiagonalMatriz n 0 matriz