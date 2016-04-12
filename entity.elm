module Entity where

import Graphics.Collage exposing (..)
import Color exposing (..)
import Component exposing (..)

type Role = Cursor | Turtle | Labeler
type alias Interaction = (Role, Role)

type alias Entity =
  { space: Spatial
  , corp: Corporeal
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
      space = Component.setVel (0, -150) self.space
    , corp = Component.setColor Color.green self.corp
    }
  else
    self

iaTurtleTurtle : Entity -> Entity -> Entity
iaTurtleTurtle self other =
  if self.label.name == "Turtle" && other.label.name == "Turtle" then
    { self |
      space = Component.setVel (0, -100) self.space
    , corp = Component.setColor Color.purple self.corp
    }
  else
    self

iaTurtleCursor : Entity -> Entity -> Entity
iaTurtleCursor self other =
  if self.label.name == "Turtle" && other.label.name == "Cursor" then
    { self |
      space = Component.setVel (100, -50) self.space
    , corp = Component.setColor Color.red self.corp
    }
  else
    self

iaNoOp : Entity -> Entity -> Entity
iaNoOp self other = self
