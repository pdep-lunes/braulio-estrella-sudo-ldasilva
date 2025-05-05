module Lib () where

import Text.Show.Functions ()

type Nombre = String
type PoderBasico = String
type SuperPoder = String
type SuperPoderActivo = Bool
type CantidadDeVida = Int

type Personaje = (Nombre, PoderBasico, SuperPoder, SuperPoderActivo, CantidadDeVida)

activarSuperPoder :: Personaje -> Personaje
activarSuperPoder (nombre, poderBasico, superPoder, _, cantidadDeVida) = (nombre, poderBasico, superPoder, True, cantidadDeVida)

desactivarSuperPoder :: Personaje -> Personaje
desactivarSuperPoder (nombre, poderBasico, superPoder, _, cantidadDeVida) = (nombre, poderBasico, superPoder, False, cantidadDeVida)

obtenerVida :: Personaje -> Int
obtenerVida (_, _, _, _, cantidadDeVida) = cantidadDeVida

tieneSuperPoderActivo :: Personaje -> Bool
tieneSuperPoderActivo (_, _, _, superPoderActivo, _) = superPoderActivo

obtenerSuperPoder :: Personaje -> SuperPoder
obtenerSuperPoder (_, _, superPoder, _, _) = superPoder

obtenerPoderBasico :: Personaje -> PoderBasico
obtenerPoderBasico (_, poderBasico, _, _, _) = poderBasico

--

quitarVida :: Int -> Personaje -> Personaje
quitarVida aRestar (nombre, poderBasico, superPoder, superPoderActivo, cantidadDeVida)
  | cantidadDeVida <= aRestar = (nombre, poderBasico, superPoder, superPoderActivo, 0)
  | otherwise = (nombre, poderBasico, superPoder, superPoderActivo, cantidadDeVida-aRestar)

matarPersonaje :: Personaje -> Personaje
matarPersonaje personaje = quitarVida (obtenerVida personaje) personaje

sumarVida :: Personaje -> Int -> Personaje
sumarVida (nombre, poderBasico, superPoder, superPoderActivo, cantidadDeVida) aSumar = (nombre, poderBasico, superPoder, superPoderActivo, cantidadDeVida + aSumar)

bolaEspinosa :: Personaje -> Personaje
bolaEspinosa = quitarVida 1000

---

esTuercaSanadora :: String -> Bool
esTuercaSanadora =  (==) "sanadora"

esTuercaDaninas :: String -> Bool
esTuercaDaninas =  (==) "dañinas"

lluviaDeTuercas :: String -> Personaje -> Personaje
lluviaDeTuercas tipoDeTuercas personaje
  | esTuercaSanadora tipoDeTuercas = sumarVida personaje 800
  | esTuercaDaninas tipoDeTuercas = quitarVida ((div 2).obtenerVida $ personaje) personaje

---

agregarEnNombreEspinas :: Personaje -> Personaje
agregarEnNombreEspinas (nombre, poderBasico, superPoder, superPoderActivo, cantidadDeVida) = (nombre ++ " Espinas estuvo aqui", poderBasico, superPoder, superPoderActivo, cantidadDeVida)

granadaDeEspinas :: Int -> Personaje -> Personaje
granadaDeEspinas radio contrincante
  | radio > 3 && estaEnLasUltimas contrincante = (agregarEnNombreEspinas . desactivarSuperPoder . matarPersonaje) contrincante
  | radio > 3 = agregarEnNombreEspinas contrincante
  | otherwise = bolaEspinosa contrincante

---

torretaCurativa :: Personaje -> Personaje
torretaCurativa aliado = (activarSuperPoder.sumarVida aliado.(*2).obtenerVida) aliado

---

atacarConPoder :: Personaje -> String -> Personaje -> Personaje
atacarConPoder atacante poder atacado
  | poder == "bola espinosa" = bolaEspinosa atacado
  | poder == "lluvia de tuercas sanadoras" = lluviaDeTuercas "sanadoras" atacante
  | poder == "lluvia de tuercas dañinas" = lluviaDeTuercas "dañinas" atacado
  -- No puedo usar granada de espinas por que no se el radio
  -- | poder == "granada de espinas" = granadaDeEspinas atacado
  | poder == "torreta curativa" = torretaCurativa atacado

atacarConPoderEspecial :: Personaje -> Personaje -> Personaje
atacarConPoderEspecial personaje contrincante
  | tieneSuperPoderActivo personaje = (atacarConPoder personaje (obtenerPoderBasico personaje).atacarConPoder personaje (obtenerSuperPoder personaje)) contrincante
  | otherwise = contrincante

estaEnLasUltimas :: Personaje -> Bool
estaEnLasUltimas = (> 800) . obtenerVida

--

espina :: Personaje
espina = ("Espina", "bola de espinas", "granada de espinas de 5 metros de radio", True, 4800)

pamela :: Personaje
pamela = ("Pamela", "lluvia de tuercas sanadoras", "torreta curativa", False, 9600)
