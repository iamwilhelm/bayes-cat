module System.Renderer exposing (..)

import Maybe exposing (andThen)
import Collage

import Entity
import Entity.Role
import Entity.Cat
import Entity.Egg

import Component.Renderable

render : Entity.Model -> Maybe Collage.Form
render entity =
  Entity.getRenderable entity `andThen` routeRender entity

-- FIXME this is game specific routing. Should pass in the route mapping
-- between roles and views. Maybe we put it in Entity.Roles?
routeRender : Entity.Model -> Component.Renderable.Model -> Maybe Collage.Form
routeRender entity renderable =
  case renderable.role of
    Entity.Role.Cat ->
      Entity.Cat.view entity
    Entity.Role.Egg ->
      Entity.Egg.view entity
    _ ->
      Nothing
