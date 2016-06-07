module Entity.Platform exposing (..)

import Basics.Extra exposing (never)
import Cmd.Extra exposing (msgToCmd)
import Collage exposing (..)
import Task
import Color

import Entity
import Entity.Role
import Component

import Vec exposing (..)

-- model

init : Entity.ID -> Vec -> Entity.Model
init id pos = {
    id = id
  , components = [
      Component.spatial 1000 pos (0, 0)
    , Component.corporeal (400, 20) Color.brown
    , Component.renderable Entity.Role.Platform
    , Component.collidable Entity.Role.Platform 1
    ]
  }

-- update

type Msg =
    NoOp

reduce : Msg -> Entity.Model -> Entity.Model
reduce msg model =
  case msg of
    NoOp ->
      model

-- interaction

interact : (Entity.Role.Name, Entity.Model) -> (Entity.Role.Name, Entity.Model) -> Cmd Msg
interact (selfRole, self) (otherRole, other) =
  case otherRole of
    _ ->
      msgToCmd NoOp

-- view

view : Entity.Model -> Maybe (List Form)
view entity =
  Entity.getCorporeal entity
  |> Maybe.map (\corp ->
    [ filled corp.color <| uncurry rect corp.dim ]
  )
