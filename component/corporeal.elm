module Component.Corporeal exposing (..)

import Vec exposing (..)
import Color

type alias Model = {
    dim : Vec.Vec
  , radius : Float
  , color : Color.Color
  }

init : Vec -> Color.Color -> Model
init dim color = {
    dim = dim
  , radius = fst dim
  , color = color
  }

color : Color.Color -> Model -> Model
color newColr corporeal =
  { corporeal | color = newColr }

dim : Float -> Float -> Model -> Model
dim w h corporeal =
  { corporeal | dim = (w, h) }
