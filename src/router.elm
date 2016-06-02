module Router exposing (..)

import Collage
import Entity
import Component.Renderable

import Entity.Role
import Entity.Cat
import Entity.Egg
import Entity.Platform

renderEntity : Entity.Model -> Component.Renderable.Model -> Maybe (List Collage.Form)
renderEntity entity renderable =
  case renderable.role of
    Entity.Role.Cat ->
      Entity.Cat.view entity
    Entity.Role.Egg ->
      Entity.Egg.view entity
    Entity.Role.Platform ->
      Entity.Platform.view entity
    _ ->
      Nothing
