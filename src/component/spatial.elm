module Component.Spatial exposing (..)

import Vec exposing (..)

type alias Model = {
    mass : Float
  , forces : List Vec
  , pos : Vec    -- in units
  , vel : Vec    -- in units / centiseconds
  , velLimit : (Vec, Vec)
  , acc : Vec    -- in units / centiseconds ** 2
  , accLimit : (Vec, Vec)
  , heading : Float
  }

init : Float -> Vec -> Vec -> Model
init mass pos vel = {
    mass = mass
  , forces = []
  , pos = pos
  , vel = vel
  , velLimit = ((-20, -20), (20, 20))
  , acc = (0, 0)
  , accLimit = ((-10, -10), (10, 10))
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
  foldForces (\force total ->
    total |+ force ./ model.mass
  ) model.acc model
