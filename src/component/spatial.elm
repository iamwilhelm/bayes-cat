module Component.Spatial exposing (..)

import Vec exposing (..)

type alias Model = {
    mass : Float
  , forces : List Vec.Vec
  , pos : Vec.Vec    -- in units
  , vel : Vec.Vec    -- in units / centiseconds
  , acc : Vec.Vec    -- in units / centiseconds ** 2
  , heading : Float
  }

init : Float -> Vec -> Model
init mass pos = {
    mass = mass
  , forces = []
  , pos = pos
  , vel = (0, 0)
  , acc = (0, 0) -- TODO unused. remove
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

clearForces : Model -> Model
clearForces model =
  { model | forces = [] }

insertForce : Vec -> Model -> Model
insertForce force model =
  { model | forces = force :: model.forces }

foldForces : (Vec -> b -> b) -> b -> Model -> b
foldForces func initVal model =
  List.foldl func initVal model.forces

totalAcc : Model -> Vec
totalAcc model =
  foldForces (\force total -> total |+ force ./ model.mass) (0, 0) model
