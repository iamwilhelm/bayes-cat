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

-- system control's job is to look for all entities that are controllable
-- and execute its control component. What the control component does, is
-- defined in the enitty itself. The control component can include a function
-- and that function should be the reduce function.
byKeyboard : Component.KeyboardControl.Msg -> List Entity.Model -> List Entity.Model
byKeyboard msg entities =
  -- call control component on every entity


  case msg of
    Component.KeyboardControl.Up ->
      filterPlayerMap (\player ->

        Entity.filterMapSpatial (Component.Spatial.insertForce (0, 100)) player
      ) entities
    Component.KeyboardControl.Down ->
      filterPlayerMap (\player ->
        Entity.filterMapSpatial (\space ->
          Component.Spatial.insertForce (0, -100) space
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
