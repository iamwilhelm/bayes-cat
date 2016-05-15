module Viewport where

import Role
import Collision
import Entity exposing (Entity)
import Entity.EntityList
import Input exposing (Input)

cull : Input -> Entity.EntityList.Model -> Entity.EntityList.Model
cull input entityListModel =
  let
    entities = entityListModel.entities
    top = (toFloat <| snd input.window) / 2
    bottom = -(toFloat <| snd input.window) / 2
    left = -(toFloat <| fst input.window) / 2
    right = (toFloat <| fst input.window) / 2
  in
    { entityListModel |
      entities = List.filter (\(id, entity) ->
          exemptions entity
          || Collision.withinBounds (top, right, bottom, left) entity
        ) entities
    }

exemptions : Entity -> Bool
exemptions entity =
  case entity.role of
    Role.Pointer ->
      True
    _ ->
      False
