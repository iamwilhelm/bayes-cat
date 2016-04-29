module Entity where

import Graphics.Collage exposing (..)
import Color exposing (Color)
import Component exposing (Spatial, Corporeal, Control, View)
import Vec exposing (..)

import Signal
import Action exposing (Action, EntityAction)
import Effects exposing (Effects)
import Task exposing (Task)

import Debug

type Role = Cursor | Turtle | Labeler | Egg

type alias Interaction = (Role, Role)

type alias Entity =
  { role: Role
  , space: Spatial
  , corp: Corporeal
  , control: Control
  , view: View
  , interactions: List Interaction
  , label: Label
  }

type alias Label = {
    name : String
  , color : Color
  }

-------------- Interactions

route : Signal.Address (List Action) -> Interaction -> (Entity -> Entity -> Effects Action)
route address interaction =
  case interaction of
    (Egg, Cursor) -> iaEggCursor address
    _ -> iaNoOp

-- Application specific interactions

iaEggCursor : Signal.Address (List Action) -> Entity -> Entity -> Effects Action
iaEggCursor address self other =
  if self.label.name == "Egg" && other.label.name == "Cursor" then
    let
      da = Debug.log "address: " address
    in
      Effects.task <| Task.succeed (Action.Entity Action.Open)
  else
    Effects.none


iaNoOp : Entity -> Entity -> Effects Action
iaNoOp self other = Effects.none
