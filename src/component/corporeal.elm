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

-- TODO what if percent is less than 0 or more than 100?
grow : Int -> Model -> Model
grow percent corporeal =
  scale (1 + toFloat percent / 100) corporeal

shrink : Int -> Model -> Model
shrink percent corporeal =
  scale (toFloat percent / 100) corporeal

scale : Float -> Model -> Model
scale scalar corporeal =
  uncurry dim (corporeal.dim .* scalar) corporeal
