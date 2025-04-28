module Lib () where

import Text.Show.Functions ()

type Nombre = String
type PoderBasico = String
type SuperPoder = String
type SuperPoderActivo = Bool
type CantidadDeVida = Int

type Personaje = (Nombre, PoderBasico, SuperPoder, SuperPoderActivo, CantidadDeVida)
type Equipo = (Personaje, Personaje)


obtenerVida :: Personaje -> Int
obtenerVida (_, _, _, _, cantidadDeVida) = cantidadDeVida

tieneSuperPoderActivo :: Personaje -> Bool
tieneSuperPoderActivo (_, _, superPoderActivo, _) = superPoderActivo

---

atacarConPoderEspecial :: Personaje -> Equipo -> Equipo
atacarConPoderEspecial personaje contrincantes
  | tieneSuperPoderActivo personaje = atacaContrincantes personaje contrincantes
  | otherwise = contrincantes

estaEnLasUltimas :: Personaje -> Bool
estaEnLasUltimas = (> 800) . obtenerVida

--

quitarVida :: Int -> Personaje -> Personaje
quitarVida aRestar (nombre, poderBasico, superPoder, superPoderActivo, cantidadDeVida)
  | cantidadDeVida <= aRestar = (nombre, poderBasico, superPoder, superPoderActivo, 0)
  | otherwise = (nombre, poderBasico, superPoder, superPoderActivo, cantidadDeVida-aRestar)

matarPersonaje :: Personaje -> Personaje
matarPersonaje = quitarVida (obtenerVida personaje)

sumarVida :: Personaje -> Int -> Personaje
sumarVida (nombre, poderBasico, superPoder, superPoderActivo, cantidadDeVida) aSumar = (nombre, poderBasico, superPoder, superPoderActivo, cantidadDeVida + aSumar)

bolaEspinosa :: Personaje -> Personaje
bolaEspinosa = quitarVida 1000
