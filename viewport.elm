module Viewport where

import Role
import Collision
import Entity exposing (Entity)
import Input exposing (Input)

cull : Input -> List Entity -> List Entity
cull input entities =
  let
    top = (toFloat <| snd input.window) / 2
    bottom = -(toFloat <| snd input.window) / 2
    left = -(toFloat <| fst input.window) / 2
    right = (toFloat <| fst input.window) / 2
  in
    List.filter (\entity ->
        exemptions entity
        || Collision.withinBounds (top, right, bottom, left) entity
      ) entities

exemptions : Entity -> Bool
exemptions entity =
  case entity.role of
    Role.Cursor ->
      True
    _ ->
      False
