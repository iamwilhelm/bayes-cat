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

--simulate : Input.Model -> Model -> Model
--simulate input entity =
--  { entity |
--    space =
--      entity.space |>
--      Component.setVel (entity.space.vel |+ entity.space.acc .* input.delta)
--      >> Component.setPos (entity.space.pos |+ entity.space.vel .* input.delta)
--  }
--
---- view
--
--view : Model -> Form
--view model =
--  case model of
--    Role.Egg entityModel ->
--      Entity.Egg.view entityModel
--    Role.Popinter entityModel ->
--      Entity.Pointer.view entityModel
--  |> move model.space.pos
--
