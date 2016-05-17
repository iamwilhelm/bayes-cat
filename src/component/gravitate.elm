module Component.Gravitate exposing (..)

import Vec exposing (..)

type alias Model = {
  acc: Vec
}

type Planet =
    ToEarth
  | ToMoon
  | ToMars

init : Planet -> Model
init planet =
  case planet of
    ToEarth ->
      { acc = (0, -0.009806) }
    ToMoon ->
      { acc = (0, -0.00162) }
    ToMars ->
      { acc = (0, -0.00371) }

acc : Vec -> Model -> Model
acc (x, y) model =
  { model | acc = (x, y) }
