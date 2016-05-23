module Entity.Egg exposing (..)

import Collage exposing (..)
import Color

import Entity
import Entity.Role
import Component
import Component.Gravitate
import Vec exposing (..)

import Debug

-- model

init : Entity.ID -> Entity.Model
init id = {
    id = id
  , components = [
      Entity.spatial 10 (0, 0)
    , Entity.corporeal (35, 35) Color.gray
    , Entity.gravitate Component.Gravitate.ToMoon
    , Entity.viewable Entity.Role.Egg
    ]
  }

-- update

type Msg =
    Open
  | Kick

reduce : Msg -> Entity.Model -> Entity.Model
reduce action model =
  case action of
    Open ->
      model
    Kick ->
      model

-- view

view : Entity.Model -> Maybe Form
view entity =
  let
    maybeSpace = Component.getSpatial entity.components
    maybeCorp = Component.getCorporeal entity.components
  in
    case (maybeSpace, maybeCorp) of
      (Just space, Just corp) ->
        Just
        <| move space.pos
          <| group [
            filled corp.color <| circle ((fst corp.dim) / 2)
          ]
      _ ->
        Nothing
