module Entity.Egg where

import Entity exposing (Entity)
import Component exposing (Spatial, Corporeal, Control, View)

import Graphics.Collage exposing (..)
import Color exposing (Color)

import Signal
import Action exposing (Action, EggAction)
import Vec exposing (..)
import Random

create : Vec -> Vec -> Entity
create pos vel = {
    role = Entity.Egg
  , space = Component.createSpatial pos vel (0, 0)
  , corp = Component.createCorporeal (35, 35) Color.gray
  , control = \input space -> space
  , view = \corp ->
      group [
        filled corp.color <| circle ((fst corp.dim) / 2)
      ]
  , label = { name = "Egg", color = Color.black }
  }

actionate : EggAction -> Entity -> Entity
actionate action entity =
  case action of
    Action.Boom ->
      { entity | corp = Component.setColor Color.blue entity.corp }
    Action.Whoa ->
      { entity | corp = Component.setColor Color.orange entity.corp }
