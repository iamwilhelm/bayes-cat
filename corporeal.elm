module Corporeal where

import Vec exposing (..)
import Color exposing (..)

type alias Corporeal = {
    color: Color
  , dim: Vec
  , radius: Float
  }

initCorporeal : Corporeal
initCorporeal = {
    dim = (10, 10)
  , radius = 5
  , color = Color.darkGray
  }

createCorporeal : Vec -> Color -> Corporeal
createCorporeal dim color = {
    dim = dim
  , radius = fst dim
  , color = color
  }

setColor : Color -> Corporeal -> Corporeal
setColor newColr corporeal =
  { corporeal | color = newColr }

setDim : Float -> Float -> Corporeal -> Corporeal
setDim w h corporeal =
  { corporeal | dim = (w, h) }
