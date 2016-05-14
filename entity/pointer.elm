module Entity.Pointer where

import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Color exposing (Color)
import Text

import Role
import Component exposing (Spatial, Corporeal, Label)
import Input

-- model

type alias Model = {
    role : Role.Role
  , space : Spatial
  , corp : Corporeal
  , label : Label
  }

init : Model
init = {
    role = Role.Pointer
  , space = Component.createSpatial (0, 0) (0, 0) (0, 0)
  , corp = Component.createCorporeal (15, 15) Color.darkGray
  , label = { name = "Pointer", color = Color.black }
  }

mapSpatial : (Spatial -> Spatial) -> Model -> Model
mapSpatial func model =
  { model | space = func model.space }

-- update

type Action = NoOp

reduce : Action -> Model -> Model
reduce action cursor =
  case action of
    NoOp ->
      cursor

control : Input.Model -> Model -> Model
control input model =
  { model | space = Component.setPos input.mouse model.space }

-- view

view : Model -> Form
view model =
  filled model.corp.color <| ngon 3 (fst model.corp.dim)
