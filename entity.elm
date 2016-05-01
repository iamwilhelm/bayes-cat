module Entity where

import Graphics.Collage exposing (..)
import Color exposing (Color)
import Component exposing (Spatial, Corporeal, Control, View)
import Vec exposing (..)
import Input exposing (Input)

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

-- update

actionate : EntityAction -> Entity -> Entity
actionate action entity =
  case action of
    Action.Open ->
      { entity | corp = Component.setColor Color.blue entity.corp }
    Action.Explode ->
      { entity | corp = Component.setColor Color.orange entity.corp }

control : Input -> Entity -> Entity
control input entity =
  { entity |
    space = entity.control input entity.space
  }

simulate : Input -> Entity -> Entity
simulate input entity =
  { entity |
    space =
      entity.space |>
      Component.setVel (entity.space.vel |+ entity.space.acc .* input.delta)
      >> Component.setPos (entity.space.pos |+ entity.space.vel .* input.delta)
  }

-- view

view : Entity -> Form
view entity =
  move entity.space.pos <| entity.view entity.corp
