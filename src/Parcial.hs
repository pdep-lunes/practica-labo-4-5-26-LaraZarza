pdePerritos.hs

Juguetes :: [Juguetes] 

data Perritos = unPerrito {
    raza :: String,
    juguetesFavoritos :: [Juguete],
    tiempoEnGuarderia :: Int, //se mide en minutos
    energia :: Int,
}


modificarEnergia :: Int -> Perritos -> Perritos
modificarEnergia unaFuncion unPerrito = unPerrito {energia = max 0 . unaFuncion . energia $ unPerrito}

jugar :: Perritos -> Perritos 
jugar unPerrito = modificarEnergia (-10) unPerrito
 
type Ladridos = Int
ladrar :: Ladridos -> Perritos -> Perritos
ladrar  unPerrito unosLadridos = modificarEnergia . div ´2´ unosLadridos $ unPerrito

raza :: Perritos -> String
raza (raza, _, _, _) = raza
esDalmata :: Perritos -> Bool
esDalmata unPerrito = raza unPerrito == "Dalmata"
esPomerania :: Perritos -> Bool
esPomerania unPerrito = raza unPerrito == "Pomerania"
esDeRazaExtravagante :: Perritos -> Bool
esDeRazaExtravagante unPerrito = esDalmata unPerrito || esPomerania unPerrito

esDiaDeSpa :: Perritos -> Bool
esDiaDeSpa unPerrito = TiempoEnGuarderia unPerrito >=50 && esDeRazaExtravagante unPerrito == "True" 
regalarJuguete :: Juguetes -> Juguetes
regalarJuguete unJuguete [unosJuguetes] = unJuguete ++ [unosJuguetes] 
diaDeSpa 
|esDiaDeSpa unPerrito = regalarJuguete "peine de goma" && modificarEnergia (==100) unPerrito
|otherwise  = unPerrito 

perderPrimerJuguete :: Perritos -> Perritos
perderPrimerJuguete unPerrito = drop 1 . juguetesFavoritos $ unPerrito
diaDeCampo :: Perritos -> Perritos 
diaDeCampo unPerrito = perderPrimerJuguete . jugar $ unPerrito

Zara :: Perritos
Zara = unPerrito "Dalmata" ["Pelota", "Mantita"] 90 80

Guarderias :: [Guarderias]
Ejercicio :: String
Tiempo :: Int

data Guarderias = UnaGuarderia {
    Nombre :: String,
    [Rutina] :: [(Ejercicio, Tiempo)]
} 

GuarderiaPdePerritos :: Guarderias
GuarderiaPdePerritos UnaGuarderia
    "GuarderíaPdePerritos" 
    [(jugar, 30), (ladrar, 20), (regalarPelota, 0), (spa, 120), (campo, 720)]


//composicion de map con + sumando los tiempos de rutina y que sea <= a tiempoEnGuarderia
HabilitadoAEstar :: Int -> Perritos -> Bool
HabilitadoAEstar sumaDeTiempos unPerrito = sum . map $ GuarderiaPdePerritos (_,(_,Tiempo)) <= tiempoEnGuarderia unPerrito

PerrosResponsables :: Perritos -> Bool //componer con dia de campo
