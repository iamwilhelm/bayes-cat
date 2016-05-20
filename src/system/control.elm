module System.Control exposing (..)

import Component.KeyboardControl
import Component.Spatial

import Entity

filterPlayerMap : (Entity.Model -> Entity.Model) -> List Entity.Model -> List Entity.Model
filterPlayerMap func entities =
  List.map (\entity ->
    if entity.id == 1 then
      func entity
    else
      entity
  ) entities

reduce : Component.KeyboardControl.Msg -> List Entity.Model -> List Entity.Model
reduce msg entities =
  case msg of
    Component.KeyboardControl.Up ->
      filterPlayerMap (\player ->
        Entity.filterMapSpatial (\space ->
          Component.Spatial.insertForce (0, 100) space
        ) player
      ) entities
    Component.KeyboardControl.Down ->
      filterPlayerMap (\player ->
        Entity.filterMapSpatial (\space ->
          Component.Spatial.insertForce (0, 100) space
        ) player
      ) entities
    Component.KeyboardControl.Left ->
      filterPlayerMap (\player ->
        Entity.filterMapSpatial (\space ->
          Component.Spatial.insertForce (-50, 0) space
        ) player
      ) entities
    Component.KeyboardControl.Right ->
      filterPlayerMap (\player ->
        Entity.filterMapSpatial (\space ->
          Component.Spatial.insertForce (50, 0) space
        ) player
      ) entities
