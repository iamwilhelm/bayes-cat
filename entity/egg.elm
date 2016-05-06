module Entity.Egg where

import Graphics.Collage exposing (..)
import Color exposing (Color)
import Signal
import Random

import Vec exposing (..)
import Role
import Component exposing (Spatial, Corporeal, Control, View, Label)

type Action = Open | Kick

type alias Egg =
  { role: Role.Role
  , space: Spatial
  , corp: Corporeal
  , control: Control
  , view: View
  , label: Label
  }

create : Vec -> Vec -> Egg
create pos vel = {
    role = Role.Egg
  , space = Component.createSpatial pos vel (0, 0)
  , corp = Component.createCorporeal (35, 35) Color.gray
  , control = \input space -> space
  , view = \corp ->
      group [
        filled corp.color <| circle ((fst corp.dim) / 2)
      ]
  , label = { name = "Egg", color = Color.black }
  }

reduce : Action -> Egg -> Egg
reduce action entity =
  case action of
    Open ->
      { entity | corp = Component.setColor Color.orange entity.corp }
    Kick ->
      { entity | corp = Component.setColor Color.blue entity.corp }
