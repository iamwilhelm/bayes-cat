module Entity.Egg exposing (..)

import Collage exposing (..)
import Random
import Color

import Entity
import Input
import Vec exposing (..)

import Debug

-- model

init : Entity.ID -> Entity.Model
init id = {
    id = id
  , components = [
      Entity.spatial (0, 0) (0, 0) (0, 0)
    , Entity.corporeal (35, 35) Color.gray
    , Entity.viewable view
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
    maybeCorp = Entity.getCorporeal entity
  in
    case maybeCorp of
      Just corp ->
        group [
          filled corp.color <| circle ((fst corp.dim) / 2)
        ]
      Nothing ->
        group []
