module Component exposing (..)

import Collage exposing (..)
import Color

import Entity.Role
import Component.Spatial
import Component.Corporeal
import Component.Label
import Component.Gravitate
import Component.Renderable
import Component.Controllable
import Component.Collidable

import Vec exposing (..)

type Model =
    SpatialType Component.Spatial.Model
  | CorporealType Component.Corporeal.Model
  | LabelType Component.Label.Model
  | GravitateType Component.Gravitate.Model
  | RenderableType Component.Renderable.Model
  | ControllableType Component.Controllable.Model
  | CollidableType Component.Collidable.Model

-- component creation

spatial : Float -> Vec -> Vec -> Model
spatial mass pos vel =
  SpatialType <| Component.Spatial.init mass pos vel

corporeal : Vec -> Color.Color -> Model
corporeal dim color =
  CorporealType <| Component.Corporeal.init dim color

label : String -> Color.Color -> Model
label title color =
  LabelType <| Component.Label.Model title color

gravitate : Component.Gravitate.Planet -> Model
gravitate planet =
  GravitateType <| Component.Gravitate.init planet

renderable: Entity.Role.Name -> Model
renderable role =
  RenderableType <| Component.Renderable.init role

controllable : Entity.Role.Name -> Model
controllable role =
  ControllableType <| Component.Controllable.init role

collidable : Entity.Role.Name -> Float -> Float -> Model
collidable role restitution durability =
  CollidableType <| Component.Collidable.init role restitution durability

-- accessors

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

getRenderable : List Model -> Maybe Component.Renderable.Model
getRenderable components =
  List.filterMap (\component ->
    case component of
      RenderableType renderable ->
        Just renderable
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

getCollidable : List Model -> Maybe Component.Collidable.Model
getCollidable components =
  List.filterMap (\component ->
    case component of
      CollidableType collide ->
        Just collide
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

filterMapCollidable : (Component.Collidable.Model -> Component.Collidable.Model) -> List Model -> List Model
filterMapCollidable func components =
  List.map (\component ->
    case component of
      CollidableType coll ->
        CollidableType (func coll)
      _ ->
        component
  ) components
