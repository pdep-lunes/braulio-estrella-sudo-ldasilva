module Lib () where

import Text.Show.Functions ()

type Poder = Personaje -> Personaje

data Personaje = UnPersonaje {
  nombre :: String,
  poderBasico :: Poder,
  superPoder :: Poder,
  superPoderActivo :: Bool,
  cantidadDeVida :: Int
}

desactivarSuperPoder :: Personaje -> Personaje
desactivarSuperPoder personaje = personaje { superPoderActivo = False }

activarSuperPoder :: Personaje -> Personaje
activarSuperPoder personaje = personaje { superPoderActivo = False }

--

matarPersonaje :: Personaje -> Personaje
matarPersonaje personaje = personaje { cantidadDeVida = 0 }

quitarVida :: Int -> Personaje -> Personaje
quitarVida aRestar personaje
  | cantidadDeVida personaje <= aRestar = matarPersonaje personaje
  | otherwise = personaje { cantidadDeVida = cantidadDeVida personaje - aRestar}

bolaEspinosa :: Personaje -> Personaje
bolaEspinosa = quitarVida 1000

---

lluviaDeTuercas :: String -> Personaje -> Personaje
lluviaDeTuercas "sanadoras" personaje = personaje { cantidadDeVida = cantidadDeVida personaje + 800 }
lluviaDeTuercas "dañina" personaje = quitarVida (cantidadDeVida personaje `div` 2) personaje

---

agregarEnNombreEspinas :: Personaje -> Personaje
agregarEnNombreEspinas personaje = personaje { nombre = nombre personaje ++ " Espinas estuvo aqui" }

granadaDeEspinas :: Int -> Personaje -> Personaje
granadaDeEspinas radio contrincante
  | radio > 3 && estaEnLasUltimas contrincante = (agregarEnNombreEspinas . desactivarSuperPoder . matarPersonaje) contrincante
  | radio > 3 = agregarEnNombreEspinas contrincante
  | otherwise = bolaEspinosa contrincante

---

duplicarVida :: Personaje -> Personaje
duplicarVida personaje = personaje { cantidadDeVida = cantidadDeVida personaje * 2}

torretaCurativa :: Personaje -> Personaje
torretaCurativa = activarSuperPoder.duplicarVida

---

estaEnLasUltimas :: Personaje -> Bool
estaEnLasUltimas = (< 800) . cantidadDeVida

--

espina :: Personaje
espina = UnPersonaje {nombre = "Espina", poderBasico = bolaEspinosa, superPoder = granadaDeEspinas 5, superPoderActivo = True, cantidadDeVida = 4800}

pamela :: Personaje
pamela = UnPersonaje {nombre="Pamela", poderBasico=lluviaDeTuercas "sanadoras", superPoder = torretaCurativa, superPoderActivo = False, cantidadDeVida = 9600}
