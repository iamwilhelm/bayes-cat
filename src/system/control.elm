module System.Control exposing (..)

import Component.Control

import Entity
import Entity.Role


{-| system control's job is to look for all entities that are controllable
 and execute its control component. What the control component does, is
 defined in the enitty itself. The control component can include a function
 and that function should be the reduce function.
-}
reduceMap : Entity.Role.Name -> (Entity.Model -> Entity.Model) -> List Entity.Model -> List Entity.Model
reduceMap role func entities =
  List.map (\entity ->
    let
      maybeControllable = Entity.getControllable entity
    in
      case maybeControllable of
        Just ctrl ->
          -- FIXME game specific
          if ctrl.role == role then
            func entity
          else
            entity
        Nothing ->
          entity
    --(Entity.getControllable entity `andThen`
    --  isRole Entity.Role.Cat `andThen`
    --  func) entity
  ) entities
