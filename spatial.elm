module Spatial where

import Vec exposing (..)

type alias Spatial = {
    pos: Vec
  , vel: Vec
  , acc: Vec
  , heading: Float
  }

initSpatial : Spatial
initSpatial = {
    pos = (0, 0)
  , vel = (0, -300)
  , acc = (0, 0)
  , heading = 0
  }

createSpatial : Vec -> Spatial
createSpatial pos = {
    pos = pos
  , vel = (0, -300)
  , acc = (0, 0)
  , heading = 0
  }

setPos : Vec -> Spatial -> Spatial
setPos newPos spatial =
  { spatial | pos = newPos }

setVel : Vec -> Spatial -> Spatial
setVel newVel spatial =
  { spatial | vel = newVel }
