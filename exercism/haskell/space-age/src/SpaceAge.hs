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

ageOn :: Planet -> Float -> Float
ageOn Earth age = age / earthYear
ageOn Mercury age = age / (earthYear * 0.2408467)
ageOn Venus age = age / (earthYear * 0.61519726)
ageOn Mars age = age / (earthYear * 1.8808158)
ageOn Jupiter age = age / (earthYear * 11.862615)
ageOn Saturn age = age / (earthYear * 29.447498)
ageOn Uranus age = age / (earthYear * 84.016846)
ageOn Neptune age = age / (earthYear * 164.79132)
