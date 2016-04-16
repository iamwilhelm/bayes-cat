module Entity where

import Graphics.Collage exposing (..)
import Color exposing (Color)
import Component exposing (Spatial, Corporeal, Control, View)
import Vec exposing (..)

type Role = Cursor | Turtle | Labeler | Egg

type alias Interaction = (Role, Role)

type alias Entity =
  { space: Spatial
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

route : Interaction -> (Entity -> Entity -> Entity)
route interaction =
  case interaction of
    (Turtle, Labeler) -> iaTurtleLabeler
    (Turtle, Turtle) -> iaTurtleTurtle
    (Turtle, Cursor) -> iaTurtleCursor
    (Labeler, Turtle) -> iaLabelerTurtle
    (Egg, Egg) -> iaEggEgg
    (Egg, Cursor) -> iaEggCursor
    _ -> iaNoOp

-- Application specific interactions

iaTurtleLabeler : Entity -> Entity -> Entity
iaTurtleLabeler self other =
  if self.label.name == "Turtle" && other.label.name == "Bomb" then
    { self |
      corp = (Component.setColor Color.red) self.corp
    }
  else if self.label.name == "Turtle" && other.label.name == "Ticking" then
    { self |
      corp = (Component.setColor Color.orange) self.corp
    }
  else if self.label.name == "Turtle" && other.label.name == "Not Ticking" then
    { self |
      corp = (Component.setColor Color.yellow) self.corp
    }
  else
    self

iaTurtleTurtle : Entity -> Entity -> Entity
iaTurtleTurtle self other =
  if self.label.name == "Turtle" && other.label.name == "Turtle" then
    self
    --{ self |
    --  space = Component.setVel (0, -100) self.space
    --, corp = Component.setColor Color.purple self.corp
    --}
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

iaLabelerTurtle : Entity -> Entity -> Entity
iaLabelerTurtle self other =
  if self.label.name == "Bomb" && other.label.name == "Turtle" then
    self
  else if self.label.name == "Ticking" && other.label.name == "Turtle" then
    self
  else if self.label.name == "Not Ticking" && other.label.name == "Turtle" then
    self
  else
    self

iaEggEgg : Entity -> Entity -> Entity
iaEggEgg self other =
  if self.label.name == "Egg" && other.label.name == "Egg" then
    { self |
      corp = Component.setColor Color.blue self.corp
    }
  else
    self

iaEggCursor : Entity -> Entity -> Entity
iaEggCursor self other =
  if self.label.name == "Egg" && self.label.name == "Cursor" then
    { self |
      space = Component.setVel (Vec.neg self.space.vel) self.space
    }
  else
    self


iaNoOp : Entity -> Entity -> Entity
iaNoOp self other = self
