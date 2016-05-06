module Entity where

import Graphics.Collage exposing (..)
import Color exposing (Color)
import Component exposing (Spatial, Corporeal, Control, View, Label)
import Vec exposing (..)
import Input exposing (Input)

import Signal
import Effects exposing (Effects)
import Task exposing (Task)

import Action exposing (Action)
import Role

import Debug

type alias Entity =
  { role: Role.Role
  , space: Spatial
  , corp: Corporeal
  , control: Control
  , view: View
  , label: Label
  }

-- update

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
