sumarIEnDiagonal :: Integer -> Integer-> Integer-> [Integer] -> [Integer]
sumarIEnDiagonal _ _ _ [] = []
sumarIEnDiagonal i posActual esDiagonal (x:xs)
    | posActual == esDiagonal = i+x : xs
    | otherwise = x : sumarIEnDiagonal i (posActual+1) esDiagonal xs

sumarEnDiagonalMatriz :: Integer -> Integer-> Integer-> [[Integer]] -> [[Integer]]
sumarEnDiagonalMatriz _ _ _ [] = []
sumarEnDiagonalMatriz numero posActual diagonal (x:xs)
    | posActual == diagonal = sumarIEnDiagonal numero posActual diagonal x : sumarEnDiagonalMatriz numero (posActual+1) posActual xs
    | posActual /= diagonal = x : sumarEnDiagonalMatriz numero (posActual+1) posActual xs

diagonalSumada :: Integer -> [[Integer]] -> [[Integer]]
diagonalSumada numero lista = sumarEnDiagonalMatriz numero 0 0 lista

type Nombre = [Char]
type Nota = Integer
type Examen = Bool -- True=Parcial -- False=Recu
type Estudiante = (Nombre,Nota,Examen)
type EstudiantesLista = [Estudiante]
elNombre :: Estudiante -> Nombre
elNombre (nombre,_,_) = nombre

laNota :: Estudiante -> Nota
laNota (_,nota,_) = nota

tipoDeExamen :: Estudiante -> Examen
tipoDeExamen (_,_,examen) = examen

aprobaronElParcial :: EstudiantesLista -> Integer -> EstudiantesLista
aprobaronElParcial [] _ = []
aprobaronElParcial (x:xs) umbral
    | laNota(x)>=umbral && tipoDeExamen(x) = x : aprobaronElParcial xs umbral
    | otherwise = aprobaronElParcial xs umbral
type Materia= [Char]
type AñoAprobacion=Integer
type Cuatrimestre=Integer --0 verano 1 primerCuatri 2 segundoCuatri
type Cursada = (Materia,AñoAprobacion, Cuatrimestre)
type CursadasTotales = [Cursada]
type CursadaAprobadas = [Cursada]
laMateria :: Cursada -> Materia
laMateria (materia,_,_) = materia

elAñoDeAprobacion :: Cursada -> AñoAprobacion
elAñoDeAprobacion (_,añoAprobacion,_) = añoAprobacion

elCuatriAprobado :: Cursada -> Cuatrimestre
elCuatriAprobado (_,_,cuatriAprobado) = cuatriAprobado

cursadasVencidas :: CursadasTotales -> CursadaAprobadas
cursadasVencidas [] = [] 
cursadasVencidas (x:xs)
    | elAñoDeAprobacion(x)<2021 || (elAñoDeAprobacion(x)== 2021 && elCuatriAprobado(x)==0) = x: cursadasVencidas xs
    | otherwise = cursadasVencidas xs



--saturarHastaUmbralNegativo :: [Integer]-> 
cantDigitosPares :: Integer -> Integer
cantDigitosPares 0 = 0
cantDigitosPares n
    | mod (mod n 10) 2 == 0 = 1 + cantDigitosPares (div n 10)
    | otherwise             = cantDigitosPares (div n 10)