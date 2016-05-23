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
import Component.Controllable

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

viewable : (List Component.Model -> Form) -> Component.Model
viewable func =
  Component.ViewableType { func = func }

controllable : Entity.Role.Name -> Component.Model
controllable role =
  Component.ControllableType <| Component.Controllable.init role

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

getViewable : Model -> Maybe Component.ViewableModel
getViewable model =
  Component.getViewable model.components

getControllable : Model -> Maybe Component.Controllable.Model
getControllable model =
  Component.getControllable model.components

filterMapSpatial : (Component.Spatial.Model -> Component.Spatial.Model) -> Model -> Model
filterMapSpatial func model =
  { model | components = Component.filterMapSpatial func model.components }

filterMapCorporeal : (Component.Corporeal.Model -> Component.Corporeal.Model) -> Model -> Model
filterMapCorporeal func model =
  { model | components = Component.filterMapCorporeal func model.components }

-- system calls

view : Model -> Maybe Form
view entity =
  let
    maybeViewable = getViewable entity
  in
    case maybeViewable of
      Just viewable ->
        Just <| viewable.func entity.components
      Nothing ->
        Nothing


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
