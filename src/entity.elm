module Entity exposing (..)

import Window
import Collage exposing (..)
import Color

import Component.Spatial
import Component.Corporeal
import Component.Label
import Component.Gravitate

import Vec exposing (..)
import Input
import Debug


type alias ID = Int

type alias Model = {
    id : ID
  , components : List Component
  }

type Component =
    Spatial Component.Spatial.Model
  | Corporeal Component.Corporeal.Model
  | Label Component.Label.Model
  | Viewable ComponentView
  | Gravitate Component.Gravitate.Model

type alias ComponentView = {
    func : Model -> Form
  }

type alias ComponentReduce = {
    func : Model -> Model
  }

-- component

spatial : Float -> Vec -> Component
spatial mass pos =
  Spatial <| Component.Spatial.init mass pos

corporeal : Vec -> Color.Color -> Component
corporeal dim color =
  Corporeal <| Component.Corporeal.init dim color

label : String -> Color.Color -> Component
label title color =
  Label <| Component.Label.Model title color

viewable : (Model -> Form) -> Component
viewable func =
  Viewable { func = func }

gravitate : Component.Gravitate.Planet -> Component
gravitate planet =
  Gravitate <| Component.Gravitate.init planet

-- accessors

getSpatial : Model -> Maybe Component.Spatial.Model
getSpatial model =
  List.filterMap (\component ->
    case component of
      Spatial spaceModel ->
        Just spaceModel
      _ ->
        Nothing
  ) model.components
  |> List.head

getCorporeal : Model -> Maybe Component.Corporeal.Model
getCorporeal model =
  List.filterMap (\component ->
    case component of
      Corporeal corpModel ->
        Just corpModel
      _ ->
        Nothing
  ) model.components
  |> List.head

getViewable : Model -> Maybe ComponentView
getViewable model =
  List.filterMap (\component ->
    case component of
      Viewable exec ->
        Just exec
      _ ->
        Nothing
  ) model.components
  |> List.head

getGravitate : Model -> Maybe Component.Gravitate.Model
getGravitate model =
  List.filterMap (\component ->
    case component of
      Gravitate grav ->
        Just grav
      _ ->
        Nothing
  ) model.components
  |> List.head

filterMapSpatial : (Component.Spatial.Model -> Component.Spatial.Model) -> Model -> Model
filterMapSpatial func entity =
  { entity |
    components = List.map (\component ->
        case component of
          Spatial space ->
            Spatial (func space)
          _ ->
            component
      ) entity.components
  }

-- system calls

view : Model -> Maybe Form
view entity =
  let
    maybeViewable = getViewable entity
  in
    case maybeViewable of
      Just viewable ->
        Just <| viewable.func entity
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
        Component.Spatial.vel (Vec.neg space.vel) space
      else
        space
  ) model

gravity : Float -> Model -> Model
gravity dt entity =
  let
    maybeGrav = getGravitate entity
  in
    case maybeGrav of
      Just grav ->
        filterMapSpatial (\space ->
          Component.Spatial.acc (grav.acc .* dt) space
        ) entity
      Nothing ->
        entity

{-| Apply newtonian physics to the entity.

Uses the Velocity Verlet integration to calculate the physics
-}
newtonian : Float -> Model -> Model
newtonian dt entity =
  filterMapSpatial (\space ->
    let
      acc = Component.Spatial.totalAcc space
      space2 = Component.Spatial.vel (space.vel |+ acc .* (dt / 10)) space
    in
      Component.Spatial.pos (space2.pos |+ ((space.vel |+ space2.vel) .* (0.5 * (dt / 10)))) space2
  ) entity

clearForces : Model -> Model
clearForces entity =
  filterMapSpatial Component.Spatial.clearForces entity


--control : Input.Model -> Entity a -> Entity b
--control input entity =
--  map (\model ->
--      { model |
--        space =
--      }
--    ) entity
--
--  { entity |
--    space = entity.control input entity.space
--  }
