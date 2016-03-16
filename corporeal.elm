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

setColor : Corporeal -> Color -> Corporeal
setColor corporeal newColr =
  { corporeal | color = newColr }

setDim : Corporeal -> Float -> Float -> Corporeal
setDim corporeal w h =
  { corporeal | dim = (w, h) }
