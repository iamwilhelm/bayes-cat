module Entity where

import Graphics.Collage exposing (..)
import Color exposing (..)

import Spatial
import Corporeal
import Vec

import Input

type Role = Cursor | Turtle | Labeler
type alias Interaction = (Role, Role)

type alias Control = Input.Input -> Spatial.Spatial -> Spatial.Spatial

type alias CorporealView = Corporeal.Corporeal -> Form

type alias Entity =
  { space: Spatial.Spatial
  , corp: Corporeal.Corporeal
  , control: Control
  , view: CorporealView
  , interactions: List Interaction
  , label: Label
  }

type alias Label = {
    name : String
  , color : Color
  }

-------------- Interactions

route : Interaction -> (Entity -> Entity -> Entity)
route interaction =
  case interaction of
    (Turtle, Labeler) -> iaTurtleLabeler
    (Turtle, Turtle) -> iaTurtleTurtle
    (Turtle, Cursor) -> iaTurtleCursor
    _ -> iaNoOp

-- Application specific interactions

iaTurtleLabeler : Entity -> Entity -> Entity
iaTurtleLabeler self other =
  if self.label.name == "Turtle" && other.label.name == "Labeler" then
    { self |
      corp = Corporeal.setColor Color.green self.corp
    , space = Spatial.setVel (0, -150) self.space
    }
  else
    self

iaTurtleTurtle : Entity -> Entity -> Entity
iaTurtleTurtle self other =
  if self.label.name == "Turtle" && other.label.name == "Turtle" then
    { self |
      corp = Corporeal.setColor Color.purple self.corp
    , space = Spatial.setVel (0, -100) self.space
    }
  else
    self

iaTurtleCursor : Entity -> Entity -> Entity
iaTurtleCursor self other =
  if self.label.name == "Turtle" && other.label.name == "Cursor" then
    { self |
      corp = Corporeal.setColor Color.red self.corp
    , space = Spatial.setVel (100, -50) self.space
    }
  else
    self

iaNoOp : Entity -> Entity -> Entity
iaNoOp self other = self
