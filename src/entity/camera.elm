module Entity.Camera exposing (..)

import Basics.Extra exposing (never)
import Collage exposing (..)
import Task
import Color

import Entity
import Entity.Role
import Component
import Component.Spatial
import Component.Corporeal
import Component.Controllable

import Vec exposing (..)

-- Model

init : Entity.ID -> Entity.Model
init id = {
    id = id
  , components = [
      Component.spatial 10 (0, 0) (0, 0)
    , Component.corporeal (1, 1) Color.gray
    , Component.controllable Entity.Role.Camera
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

-- interact

interact : (Entity.Role.Name, Entity.Model) -> (Entity.Role.Name, Entity.Model) -> Cmd Msg
interact (selfRole, self) (otherRole, other) =
  case otherRole of
    _ ->
      Task.perform never identity (Task.succeed NoOp)

-- view

view : Entity.Model -> Maybe (List Form)
view model =
  Nothing
