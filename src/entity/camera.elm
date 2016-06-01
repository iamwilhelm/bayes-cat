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
      Component.spatial 100 (0, 0) (0, 0)
    , Component.corporeal (1, 1) Color.gray
    , Component.controllable Entity.Role.Camera
    ]
  }

-- update

type Msg =
    Move DirectionMsg
  | Zoom ScaleMsg
  | NoOp

type DirectionMsg =
    Up
  | Down
  | Left
  | Right

type ScaleMsg =
    In
  | Out

reduce : Msg -> Entity.Model -> Entity.Model
reduce msg model =
  let
    _ = Debug.log "camera reduce" msg
  in
    case msg of
      Move direction ->
        reduceMove direction model
      Zoom scale ->
        reduceZoom scale model
      _ ->
        model

reduceMove : DirectionMsg -> Entity.Model -> Entity.Model
reduceMove msg model =
  case msg of
    Up ->
      Entity.filterMapSpatial (Component.Spatial.insertForce (0, 10)) model
    Down ->
      Entity.filterMapSpatial (Component.Spatial.insertForce (0, -10)) model
    Left ->
      Entity.filterMapSpatial (Component.Spatial.insertForce (-10, 0)) model
    Right ->
      Entity.filterMapSpatial (Component.Spatial.insertForce (10, 0)) model

reduceZoom : ScaleMsg -> Entity.Model -> Entity.Model
reduceZoom msg model =
  case msg of
    In ->
      Entity.filterMapSpatial (\space ->
        { space | scale = space.scale + 0.1 }
      ) model
    Out ->
      Entity.filterMapSpatial (\space ->
        { space | scale = space.scale - 0.1 }
      ) model

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
