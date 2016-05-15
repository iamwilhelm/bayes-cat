module Entity exposing (..)

import Component.Spatial
import Component.Corporeal
import Component.Label

import Vec exposing (..)
import Collage exposing (..)
import Color
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

type alias ComponentView = {
    func : Model -> Form
  }

type alias ComponentReduce = {
    func : Model -> Model
  }

-- component

spatial : Vec -> Vec -> Vec -> Component
spatial pos vel acc =
  Spatial <| Component.Spatial.init pos vel acc

corporeal : Vec -> Color.Color -> Component
corporeal dim color =
  Corporeal <| Component.Corporeal.init dim color

label : String -> Color.Color -> Component
label title color =
  Label <| Component.Label.Model title color

viewable : (Model -> Form) -> Component
viewable func =
  Viewable { func = func }

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

newtonian : Float -> Model -> Model
newtonian dt entity =
  filterMapSpatial (\space ->
    let
      --_ = Debug.log "space: " space
      --a = Debug.log "dt: " dt

      space2 = Component.Spatial.vel (space.vel |+ space.acc .* (dt / 1000)) space
      --b = Debug.log "space2 " space2

      space3 = Component.Spatial.pos (space.pos |+ space.vel .* (dt / 1000)) space2
      --c = Debug.log "space3 " space3
    in
      space3
  ) entity

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
