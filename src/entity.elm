module Entity exposing (..)

import Window
import Collage exposing (..)
import Color

import Entity.Role
import Component
import Component.Spatial
import Component.Corporeal
import Component.Label
import Component.Gravitate
import Component.Viewable
import Component.Controllable
import Component.Collidable

import Vec exposing (..)
import Debug

type alias ID = Int

type alias Model = {
    id : ID
  , components : List Component.Model
  }

-- component creation TODO Should move into Component?

spatial : Float -> Vec -> Component.Model
spatial mass pos =
  Component.SpatialType <| Component.Spatial.init mass pos

corporeal : Vec -> Color.Color -> Component.Model
corporeal dim color =
  Component.CorporealType <| Component.Corporeal.init dim color

label : String -> Color.Color -> Component.Model
label title color =
  Component.LabelType <| Component.Label.Model title color

gravitate : Component.Gravitate.Planet -> Component.Model
gravitate planet =
  Component.GravitateType <| Component.Gravitate.init planet

viewable : Entity.Role.Name -> Component.Model
viewable role =
  Component.ViewableType <| Component.Viewable.init role

controllable : Entity.Role.Name -> Component.Model
controllable role =
  Component.ControllableType <| Component.Controllable.init role

collidable : Entity.Role.Name -> Component.Model
collidable role =
  Component.CollidableType <| Component.Collidable.init role

-- accessors

getSpatial : Model -> Maybe Component.Spatial.Model
getSpatial model =
  Component.getSpatial model.components

getCorporeal : Model -> Maybe Component.Corporeal.Model
getCorporeal model =
  Component.getCorporeal model.components

getGravitate : Model -> Maybe Component.Gravitate.Model
getGravitate model =
  Component.getGravitate model.components

getViewable : Model -> Maybe Component.Viewable.Model
getViewable model =
  Component.getViewable model.components

getControllable : Model -> Maybe Component.Controllable.Model
getControllable model =
  Component.getControllable model.components

getCollidable : Model -> Maybe Component.Collidable.Model
getCollidable model =
  Component.getCollidable model.components

getCollidablePair : Model -> Model -> (Maybe Component.Collidable.Model, Maybe Component.Collidable.Model)
getCollidablePair self other =
  ( Component.getCollidable self.components
  , Component.getCollidable other.components
  )

filterMapSpatial : (Component.Spatial.Model -> Component.Spatial.Model) -> Model -> Model
filterMapSpatial func model =
  { model | components = Component.filterMapSpatial func model.components }

filterMapCorporeal : (Component.Corporeal.Model -> Component.Corporeal.Model) -> Model -> Model
filterMapCorporeal func model =
  { model | components = Component.filterMapCorporeal func model.components }

-- system calls

boundFloor : Window.Size -> Model -> Model
boundFloor size model =
  filterMapSpatial (\space ->
    let
      (w', h') = (size.width, size.height)
      (w, h) = (toFloat w', toFloat h')
    in
      if Vec.y space.pos < -(h / 2) then
        space
        |> Component.Spatial.vel ((Vec.x space.vel, -(Vec.y space.vel)) .* 0.9)
        |> Component.Spatial.pos (Vec.x space.pos, -(h / 2))
      else
        space
  ) model
