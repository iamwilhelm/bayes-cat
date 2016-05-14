module Component.Spatial exposing (..)

import Vec exposing (..)

type alias Model = {
    pos : Vec.Vec
  , vel : Vec.Vec
  , acc : Vec.Vec
  , heading : Float
  }

init : Vec -> Vec -> Vec -> Model
init pos vel acc = {
    pos = pos
  , vel = vel
  , acc = acc
  , heading = 0
  }

pos : Vec -> Model -> Model
pos newPos spatial =
  { spatial | pos = newPos }

vel : Vec -> Model -> Model
vel newVel spatial =
  { spatial | vel = newVel }

acc : Vec -> Model -> Model
acc newAcc spatial =
  { spatial | acc = newAcc }
