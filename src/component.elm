module Component exposing (..)

import Collage exposing (..)

import Component.Spatial
import Component.Corporeal
import Component.Label
import Component.Gravitate
import Component.KeyboardControl

type Model =
    SpatialType Component.Spatial.Model
  | CorporealType Component.Corporeal.Model
  | LabelType Component.Label.Model
  | ViewableType ComponentViewable
  | GravitateType Component.Gravitate.Model
--  | Controllable ComponentControllable

{-| A component that renders the entity's view method

NOTE: Reason why Component declarations are in Entity is because
a view component refers back to the entity model itself, as well
as an entity containing a list of components
-}
type alias ComponentViewable = {
    func : List Model -> Form
  }

--type alias ComponentControllable = {
--    func : message -> List Component -> List Component
--  }

getSpatial : List Model -> Maybe Component.Spatial.Model
getSpatial components =
  List.filterMap (\component ->
    case component of
      SpatialType spaceModel ->
        Just spaceModel
      _ ->
        Nothing
  ) components
  |> List.head

getCorporeal : List Model -> Maybe Component.Corporeal.Model
getCorporeal components =
  List.filterMap (\component ->
    case component of
      CorporealType corpModel ->
        Just corpModel
      _ ->
        Nothing
  ) components
  |> List.head

getGravitate : List Model -> Maybe Component.Gravitate.Model
getGravitate components =
  List.filterMap (\component ->
    case component of
      GravitateType grav ->
        Just grav
      _ ->
        Nothing
  ) components
  |> List.head

getViewable : List Model -> Maybe ComponentViewable
getViewable components =
  List.filterMap (\component ->
    case component of
      ViewableType exec ->
        Just exec
      _ ->
        Nothing
  ) components
  |> List.head

--getControllable : List Model -> Maybe ComponentControllable
--getControllable components =
--  List.filterMap (\component ->
--    case component of
--      Controllable ctrl ->
--        Just ctrl
--      _ ->
--        Nothing
--  ) components
--  |> List.head

filterMapSpatial : (Component.Spatial.Model -> Component.Spatial.Model) -> List Model -> List Model
filterMapSpatial func components =
  List.map (\component ->
    case component of
      SpatialType space ->
        SpatialType (func space)
      _ ->
        component
  ) components

filterMapCorporeal : (Component.Corporeal.Model -> Component.Corporeal.Model) -> List Model -> List Model
filterMapCorporeal func components =
  List.map (\component ->
    case component of
      CorporealType corp ->
        CorporealType (func corp)
      _ ->
        component
  ) components
