module Library where
import PdePreludat

--Parte A
type Truco = Plato -> Plato

data Participante = UnParticipante {
                            nombre :: String,
                            trucosDeCocina :: [Truco],
                            especialidad :: Plato
                            } 

data Plato = UnPlato {
                                dificultad :: Number, --1 a 10
                                nombrePlato :: String,
                                componentes :: [Ingrediente]
                                } deriving (Show, Eq)

data Ingrediente = UnIngrediente {
                                nombreIngrediente :: String,
                                pesoIngrediente :: Number --en gramos
                                } deriving (Show, Eq)

agregarComponenteySuPeso :: Ingrediente -> Plato -> Plato
agregarComponenteySuPeso (UnIngrediente unNombreIngrediente unPesoIngrediente) (UnPlato unaDificultad unNombrePlato unosComponentes) = 
    UnPlato unaDificultad unNombrePlato (unosComponentes ++ [UnIngrediente unNombreIngrediente unPesoIngrediente])

endulzar :: Number -> Truco
endulzar pesoAzucar unPlato = agregarComponenteySuPeso (UnIngrediente "azucar" pesoAzucar) unPlato
-- endulzar pesoAzucar = agregarComponenteySuPeso (UnIngrediente "azucar" pesoAzucar) 

salar :: Number -> Truco
salar pesoSal unPlato = agregarComponenteySuPeso (UnIngrediente "sal" pesoSal) unPlato
-- salar pesoSal = agregarComponenteySuPeso (UnIngrediente "sal" pesoSal)

darSabor :: Number -> Number -> Truco
darSabor pesoAzucar pesoSal unPlato = endulzar pesoAzucar . salar pesoSal $ unPlato
-- darSabor pesoAzucar pesoSal = endulzar pesoAzucar . salar pesoSal
 
duplicarPorcion :: Truco
duplicarPorcion (UnPlato unaDificultad unNombrePlato unosComponentes) = UnPlato unaDificultad unNombrePlato (map duplicarIngrediente unosComponentes)

duplicarIngrediente :: Ingrediente -> Ingrediente
duplicarIngrediente (UnIngrediente unNombreIngrediente unPesoIngrediente) = UnIngrediente unNombreIngrediente (unPesoIngrediente * 2)

cantidadDeComponentes :: Plato -> Number
cantidadDeComponentes unPlato = length (componentes unPlato)

gramosIngrediente :: Ingrediente -> Number
gramosIngrediente (UnIngrediente _ unPesoIngrediente) = unPesoIngrediente 

gramosIngredienteMayorA10 :: Ingrediente -> Bool
gramosIngredienteMayorA10 unIngrediente = gramosIngrediente unIngrediente > 10

quitarComponentesSeleccionados :: [Ingrediente] -> [Ingrediente]
quitarComponentesSeleccionados unosComponentes = filter gramosIngredienteMayorA10 unosComponentes
-- quitarComponentesSeleccionados = filter gramosIngredienteMayorA10

simplificar :: Truco
simplificar unPlato
    | cantidadDeComponentes unPlato > 5 && dificultad unPlato > 7 =  UnPlato 5 (nombrePlato unPlato) (quitarComponentesSeleccionados (componentes unPlato))
    | otherwise = unPlato

componentePertenece :: String -> Ingrediente -> Bool
componentePertenece unNombre unIngrediente = nombreIngrediente unIngrediente == unNombre

noEsIngredienteVegano :: Ingrediente -> Bool
noEsIngredienteVegano unIngrediente = componentePertenece "carne" unIngrediente || componentePertenece "huevo" unIngrediente || componentePertenece "lacteo" unIngrediente

esVegano :: Plato -> Bool
esVegano unPlato = not (any noEsIngredienteVegano (componentes unPlato))

esSinTacc :: Plato -> Bool
esSinTacc unPlato = not (any (componentePertenece "harina") (componentes unPlato))

esComplejo :: Plato ->  Bool
esComplejo unPlato = dificultad unPlato > 7 && cantidadDeComponentes unPlato > 5

tieneMuchaSal :: Ingrediente -> Bool
tieneMuchaSal unIngrediente = nombreIngrediente unIngrediente == "sal" && pesoIngrediente unIngrediente > 2  

noAptoHipertension :: Plato -> Bool
noAptoHipertension unPlato = any tieneMuchaSal (componentes unPlato)

--Parte B
pepe :: Participante  
pepe = UnParticipante "Pepe Ronccino" [darSabor 5 2, simplificar, duplicarPorcion] platoPepe

platoPepe :: Plato
platoPepe = UnPlato 8 "Plato de Pepe" [UnIngrediente "sal" 3, UnIngrediente "harina" 5, UnIngrediente "carne" 15, UnIngrediente "huevo" 10, UnIngrediente "lacteo" 20, UnIngrediente "azucar" 4]

--Parte C
aplicarTruco :: Plato -> Truco -> Plato
aplicarTruco unPlato unTruco = unTruco unPlato

cocinar :: Plato -> [Truco] -> Plato
cocinar unPlato trucos = foldl aplicarTruco unPlato trucos

-- semilla= unPlato para un foldl ya que quiero aplicar n trucos seguidos sin llamar a la funcion n veces, sino 1 sola
-- funcion reduccion:  aplicarTruco

tieneMasDificultad :: Plato -> Plato -> Bool
tieneMasDificultad plato1 plato2 = dificultad plato1 > dificultad plato2

sumaDeLosPesos :: Plato -> Number 
sumaDeLosPesos unPlato = sum (map pesoIngrediente (componentes unPlato)) 

esMejorQue :: Plato -> Plato -> Bool
esMejorQue plato1 plato2 = tieneMasDificultad plato1 plato2 && sumaDeLosPesos plato1 < sumaDeLosPesos plato2

participanteEstrella :: [Participante] -> Participante
participanteEstrella [unParticipante] = unParticipante  
participanteEstrella (unParticipante : otrosParticipantes) = foldl compararParticipantes unParticipante otrosParticipantes

compararParticipantes :: Participante -> Participante -> Participante
compararParticipantes unParticipante otroParticipante
    | esMejorQue (platoCocido unParticipante) (platoCocido otroParticipante) = unParticipante 
    | otherwise = otroParticipante

platoCocido :: Participante -> Plato
platoCocido unParticipante = cocinar (especialidad unParticipante) (trucosDeCocina unParticipante)
