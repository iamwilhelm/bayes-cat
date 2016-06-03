module Entity.Cat exposing (..)

import Basics.Extra exposing (never)
import Collage exposing (..)
import Task
import Color

import Entity
import Entity.Role
import Component
import Component.Spatial
import Component.Corporeal
import Component.Gravitate
import Vec exposing (..)

import Debug

-- model

init : Entity.ID -> Entity.Model
init id = {
    id = id
  , components = [
      Component.spatial 50 (100, 0) (0, 0)
    , Component.corporeal (45, 45) Color.orange
    , Component.gravitate Component.Gravitate.ToEarth
    , Component.controllable Entity.Role.Cat
    , Component.renderable Entity.Role.Cat
    , Component.collidable Entity.Role.Cat 1 10
    ]
  }

-- update

type Msg =
    Move MsgDirection
  | Grow
  | NoOp

type MsgDirection =
    Up
  | Down
  | Left
  | Right

reduce : Msg -> Entity.Model -> Entity.Model
reduce msg model =
  case msg of
    Move direction ->
      reduceMove direction model
    Grow ->
      Entity.filterMapCorporeal (Component.Corporeal.color Color.red) model
    _ ->
      model

reduceMove : MsgDirection -> Entity.Model -> Entity.Model
reduceMove direction model =
  case direction of
    Up ->
      Entity.filterMapSpatial (Component.Spatial.insertForce (0, 1000)) model
    Down ->
      Entity.filterMapSpatial (Component.Spatial.insertForce (0, -50)) model
    Left ->
      Entity.filterMapSpatial (Component.Spatial.insertForce (-50, 0)) model
    Right ->
      Entity.filterMapSpatial (Component.Spatial.insertForce (50, 0)) model

-- interaction

-- what will other entities do to cat?
interact : (Entity.Role.Name, Entity.Model) -> (Entity.Role.Name, Entity.Model) -> Cmd Msg
interact (selfRole, self) (otherRole, other) =
  case otherRole of
    Entity.Role.Egg ->
      Task.perform never identity (Task.succeed Grow)
    _ ->
      Task.perform never identity (Task.succeed NoOp)

-- view

view : Entity.Model -> Maybe (List Form)
view entity =
  Entity.getCorporeal entity
  |> Maybe.map (\corp ->
    [ filled corp.color <| circle ((fst corp.dim) / 2)
    , viewLeftEar corp
    , viewRightEar corp
    ]
  )

viewLeftEar : Component.Corporeal.Model -> Form
viewLeftEar corp =
  move ((fst corp.dim) / 2 * cos (degrees 135), ((fst corp.dim) / 2 * sin (degrees 45)))
    <| filled corp.color <| ngon 3 ((fst corp.dim) / 4)

viewRightEar : Component.Corporeal.Model -> Form
viewRightEar corp =
  move ((fst corp.dim) / 2 * cos (degrees 45), ((fst corp.dim) / 2 * sin (degrees 45)))
    <| rotate (degrees 180)
    <| filled corp.color <| ngon 3 ((fst corp.dim) / 4)
