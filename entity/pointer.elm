module Entity.Pointer where

import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Color exposing (Color)
import Text

import Role exposing (Role)
import Component exposing (Spatial, Corporeal, Control, View, Label)

type Action = NoOp

type alias Pointer = {
    role : Role
  , space : Spatial
  , corp : Corporeal
  , control : Control
  , view : View
  , label : Label
  }

init : Pointer
init = {
    role = Role.Pointer
  , space = Component.createSpatial (0, 0) (0, 0) (0, 0)
  , corp = Component.createCorporeal (15, 15) Color.darkGray
  , control = \input space -> Component.setPos input.mouse space
  , view = \corp -> filled corp.color <| ngon 3 (fst corp.dim)
  , label = { name = "Pointer", color = Color.black }
  }

reduce : Action -> Pointer -> Pointer
reduce action cursor =
  case action of
    NoOp ->
      cursor
