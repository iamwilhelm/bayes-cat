module Component where

import Graphics.Collage exposing (..)
import Vec
import Color
import Input

-------- Spatial Component

type alias Spatial = {
    pos: Vec.Vec
  , vel: Vec.Vec
  , acc: Vec.Vec
  , heading: Float
  }

initSpatial : Spatial
initSpatial = {
    pos = (0, 0)
  , vel = (0, -300)
  , acc = (0, 0)
  , heading = 0
  }

createSpatial : Vec.Vec -> Vec.Vec -> Vec.Vec -> Spatial
createSpatial pos vel acc = {
    pos = pos
  , vel = vel
  , acc = acc
  , heading = 0
  }

setPos : Vec.Vec -> Spatial -> Spatial
setPos newPos spatial =
  { spatial | pos = newPos }

setVel : Vec.Vec -> Spatial -> Spatial
setVel newVel spatial =
  { spatial | vel = newVel }

setAcc : Vec.Vec -> Spatial -> Spatial
setAcc newAcc spatial =
  { spatial | acc = newAcc }

--------- Corporeal Component

type alias Corporeal = {
    color: Color.Color
  , dim: Vec.Vec
  , radius: Float
  }

initCorporeal : Corporeal
initCorporeal = {
    dim = (10, 10)
  , radius = 5
  , color = Color.darkGray
  }

createCorporeal : Vec.Vec -> Color.Color -> Corporeal
createCorporeal dim color = {
    dim = dim
  , radius = fst dim
  , color = color
  }

setColor : Color.Color -> Corporeal -> Corporeal
setColor newColr corporeal =
  { corporeal | color = newColr }

setDim : Float -> Float -> Corporeal -> Corporeal
setDim w h corporeal =
  { corporeal | dim = (w, h) }

---------- Control Component

type alias Control = Input.Input -> Spatial -> Spatial

---------- CorporealView Component

type alias CorporealView = Corporeal -> Form
