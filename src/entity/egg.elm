module Entity.Egg exposing (..)

import Collage exposing (..)
import Color

import Entity
import Component.Gravitate
import Input
import Vec exposing (..)

import Debug

-- model

init : Entity.ID -> Entity.Model
init id = {
    id = id
  , components = [
      Entity.spatial 10 (0, 0)
    , Entity.corporeal (35, 35) Color.gray
    , Entity.viewable view
    , Entity.gravitate Component.Gravitate.ToMoon
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
        ]
      _ ->
        group []
