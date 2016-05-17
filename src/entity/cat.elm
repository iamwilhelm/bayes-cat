module Entity.Cat exposing (..)

import Collage exposing (..)
import Color

import Entity
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
    ]
  }

-- update

type Msg =
    Move Vec.Vec
  | Kill

reduce : Msg -> Entity.Model -> Entity.Model
reduce action model =
  case action of
    Move (x, y) ->
      model
    Kill ->
      model

-- view

view : Entity.Model -> Form
view entity =
  let
    maybeSpace = Entity.getSpatial entity
    maybeCorp = Entity.getCorporeal entity
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
