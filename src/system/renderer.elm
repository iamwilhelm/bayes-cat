module System.Renderer exposing (..)

import Maybe exposing (andThen)
import Collage
import Transform exposing (Transform, multiply)

import Entity
import Entity.Role
import Entity.Cat
import Entity.Egg

import Component.Spatial
import Component.Renderable

render : Maybe Entity.Model -> Entity.Model -> Maybe Collage.Form
render camera entity =
  let
    entityForms = Entity.getRenderable entity `andThen` renderEntity entity
    tf = totalTransform camera entity
  in
    Maybe.map (Collage.groupTransform tf) entityForms

-- FIXME this is game specific routing. Should pass in the route mapping
-- between roles and views. Maybe we put it in Entity.Roles?
renderEntity : Entity.Model -> Component.Renderable.Model -> Maybe (List Collage.Form)
renderEntity entity renderable =
  case renderable.role of
    Entity.Role.Cat ->
      Entity.Cat.view entity
    Entity.Role.Egg ->
      Entity.Egg.view entity
    _ ->
      Nothing

totalTransform : Maybe Entity.Model -> Entity.Model -> Transform
totalTransform camera entity =
  (e2w entity) `multiply` (w2c camera)

e2w : Entity.Model -> Transform
e2w entity =
  Entity.getSpatial entity
  |> Maybe.map Component.Spatial.transform
  |> Maybe.withDefault Transform.identity

w2c : Maybe Entity.Model -> Transform
w2c camera =
  camera `andThen` Entity.getSpatial
  |> Maybe.map Component.Spatial.invertedControlTransform
  |> Maybe.withDefault Transform.identity
