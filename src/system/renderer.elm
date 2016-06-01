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

render : Entity.Model -> Entity.Model -> Maybe Collage.Form
render camera entity =
  let
    entityForms = Entity.getRenderable entity `andThen` renderEntity entity
    tf = totalTransform entity
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

totalTransform : Entity.Model -> Transform
totalTransform entity =
  (w2c entity) `multiply` (e2w entity)

e2w : Entity.Model -> Transform
e2w entity =
  Entity.getSpatial entity
  |> Maybe.map Component.Spatial.transform
  |> Maybe.withDefault Transform.identity

w2c : Entity.Model -> Transform
w2c camera =
  Entity.getSpatial camera
  |> Maybe.map Component.Spatial.transform
  |> Maybe.withDefault Transform.identity
