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
