module Entity.Cat exposing (..)

import Collage exposing (..)
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
      Entity.spatial 50 (100, 0)
    , Entity.corporeal (45, 45) Color.orange
    , Entity.viewable view
    , Entity.gravitate Component.Gravitate.ToEarth
    , Entity.controllable Entity.Role.Cat
    ]
  }

-- update

type Msg =
    Move MsgDirection
  | Kill

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
    Kill ->
      model

reduceMove : MsgDirection -> Entity.Model -> Entity.Model
reduceMove direction model =
  case direction of
    Up ->
      Entity.filterMapSpatial (Component.Spatial.insertForce (0, 100)) model
    Down ->
      Entity.filterMapSpatial (Component.Spatial.insertForce (0, -150)) model
    Left ->
      Entity.filterMapSpatial (Component.Spatial.insertForce (-50, 0)) model
    Right ->
      Entity.filterMapSpatial (Component.Spatial.insertForce (50, 0)) model

-- view

view : List Component.Model -> Form
view components =
  let
    maybeSpace = Component.getSpatial components
    maybeCorp = Component.getCorporeal components
  in
    case (maybeSpace, maybeCorp) of
      (Just space, Just corp) ->
        move space.pos
        <| group [
            filled corp.color <| circle ((fst corp.dim) / 2)
          , viewLeftEar corp
          , viewRightEar corp
          ]
      _ ->
        group []

viewLeftEar : Component.Corporeal.Model -> Form
viewLeftEar corp =
  move ((fst corp.dim) / 2 * cos (degrees 135), ((fst corp.dim) / 2 * sin (degrees 45)))
    <| filled corp.color <| ngon 3 ((fst corp.dim) / 4)

viewRightEar : Component.Corporeal.Model -> Form
viewRightEar corp =
  move ((fst corp.dim) / 2 * cos (degrees 45), ((fst corp.dim) / 2 * sin (degrees 45)))
    <| rotate (degrees 180)
    <| filled corp.color <| ngon 3 ((fst corp.dim) / 4)
