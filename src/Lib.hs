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

