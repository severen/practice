module SpaceAge (Planet(..), ageOn) where

data Planet
  = Earth
  | Mercury
  | Venus
  | Mars
  | Jupiter
  | Saturn
  | Uranus
  | Neptune

earthYear :: Float
earthYear = 31557600

timescaleOn :: Planet -> Float
timescaleOn Earth = earthYear
timescaleOn Mercury = earthYear * 0.2408467
timescaleOn Venus = earthYear * 0.61519726
timescaleOn Mars = earthYear * 1.8808158
timescaleOn Jupiter = earthYear * 11.862615
timescaleOn Saturn = earthYear * 29.447498
timescaleOn Uranus = earthYear * 84.016846
timescaleOn Neptune = earthYear * 164.79132

ageOn :: Planet -> Float -> Float
ageOn planet age = age / timescaleOn planet
