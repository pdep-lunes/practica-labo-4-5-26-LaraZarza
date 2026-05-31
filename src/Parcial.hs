module Parcial where
import Text.Show.Functions()

-- Parte A
type Juguetes = String

data Perritos = unPerrito {
    raza :: String,
    juguetesFavoritos :: [Juguetes],
    tiempoEnGuarderia :: Int, -- se mide en minutos
    energia :: Int
} deriving (Show, Eq)

type Ejercicio = Perritos -> Perritos

modificarEnergia :: (Int -> Int) -> Perritos -> Perritos
modificarEnergia unaFuncion unPerrito = unPerrito {energia = max 0 . unaFuncion . energia $ unPerrito}

jugar :: Ejercicio 
jugar unPerrito = modificarEnergia (\x -> x - 10) unPerrito
 
type Ladridos = Int

ladrar :: Ladridos -> Ejercicio
ladrar unosLadridos unPerrito = modificarEnergia (+ (div unosLadridos 2)) unPerrito 

regalar :: Juguetes -> Ejercicio
regalar unJuguete unPerrito = unPerrito {juguetesFavoritos = juguetesFavoritos unPerrito ++ [unJuguete]}

esDalmata :: Perritos -> Bool
esDalmata unPerrito = raza unPerrito == "Dalmata"

esPomerania :: Perritos -> Bool
esPomerania unPerrito = raza unPerrito == "Pomerania"

esDeRazaExtravagante :: Perritos -> Bool
esDeRazaExtravagante unPerrito = esDalmata unPerrito || esPomerania unPerrito

diaDeSpa :: Ejercicio
diaDeSpa unPerrito  
    |tiempoEnGuarderia unPerrito >= 50 || esDeRazaExtravagante unPerrito = regalar "peine de goma" (unPerrito {energia = 100})
    |otherwise  = unPerrito 

perderPrimerJuguete :: Perritos -> Perritos
perderPrimerJuguete unPerrito = unPerrito {juguetesFavoritos = drop 1 (juguetesFavoritos unPerrito)} 

diaDeCampo :: Ejercicio
diaDeCampo unPerrito = perderPrimerJuguete . jugar $ unPerrito

zara :: Perritos
zara = unPerrito "Dalmata" ["Pelota", "Mantita"] 90 80

type Tiempo = Int

type Rutina = (Ejercicio, Tiempo)

data Guarderias = unaGuarderia {
    nombre :: String,
    rutina :: [Rutina]
} 

guarderiaPdePerritos :: Guarderias
guarderiaPdePerritos = unaGuarderia "GuarderíaPdePerritos" [(jugar, 30), (ladrar 18, 20), (regalar "Pelota", 0), (diaDeSpa, 120), (diaDeCampo, 720)]

--Parte B

habilitadoAEstar :: Perritos -> Guarderias -> Bool
habilitadoAEstar unPerrito unaGuarderia = tiempoEnGuarderia unPerrito > tiempoDeRutina unaGuarderia

tiempoDeRutina :: Guarderias -> Int
tiempoDeRutina unaGuarderia = sum . map snd $ rutina unaGuarderia 

perrosResponsables :: Perritos -> Bool 
perrosResponsables unPerrito = (length . juguetesFavoritos . diaDeCampo $ unPerrito) > 3

perroRealizaRutina :: Perritos -> Guarderias -> Perritos
perroRealizaRutina unPerrito unaGuarderia
    | habilitadoAEstar unPerrito unaGuarderia = aplicarEjercicios (rutina unaGuarderia) unPerrito
    | otherwise = unPerrito

aplicarEjercicios :: [Rutina] -> Perritos -> Perritos
aplicarEjercicios unosEjercicios unPerrito = foldl aplicarEjercicio unPerrito unosEjercicios 

aplicarEjercicio :: Perritos -> Rutina -> Perritos
aplicarEjercicio unPerrito (unEjercicio, _) = unEjercicio unPerrito   

perroCansado :: Perritos -> Bool
perroCansado unPerro = energia unPerro < 5

perrosCansados :: [Perritos] -> Guarderias -> [Perritos]
--perrosCansados unosPerros unaGuarderia = filter perroCansado (map (\p -> perroRealizaRutina p unaGuarderia) unosPerros) 
perrosCansados unosPerros unaGuarderia = filter perroCansado . map (\p -> perroRealizaRutina p unaGuarderia) $ unosPerros
