module Component exposing (..)

import Collage exposing (..)

import Entity.Role
import Component.Spatial
import Component.Corporeal
import Component.Label
import Component.Gravitate
import Component.Viewable
import Component.Controllable

type Model =
    SpatialType Component.Spatial.Model
  | CorporealType Component.Corporeal.Model
  | LabelType Component.Label.Model
  | GravitateType Component.Gravitate.Model
  | ViewableType Component.Viewable.Model
  | ControllableType Component.Controllable.Model

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

getViewable : List Model -> Maybe Component.Viewable.Model
getViewable components =
  List.filterMap (\component ->
    case component of
      ViewableType viewable ->
        Just viewable
      _ ->
        Nothing
  ) components
  |> List.head

getControllable : List Model -> Maybe Component.Controllable.Model
getControllable components =
  List.filterMap (\component ->
    case component of
      ControllableType ctrl ->
        Just ctrl
      _ ->
        Nothing
  ) components
  |> List.head

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
