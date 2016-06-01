module System.Control exposing (..)

import Maybe exposing (andThen)

import Entity
import Entity.Role

import Component.Controllable

{-| system control's job is to look for all entities that are controllable
 and execute its control component. What the control component does, is
 defined in the enitty itself. The control component can include a function
 and that function should be the reduce function.
-}
control : Entity.Role.Name -> (Entity.Model -> Entity.Model) -> Entity.Model -> Entity.Model
control role messagedReducer entity =
  Entity.getControllable entity
    `andThen` Component.Controllable.isRole role
    |> Maybe.map (\ctrl ->
        if (ctrl.role == Entity.Role.Camera) then
          let
            _ = Debug.log "result entity" result
            result = messagedReducer entity
          in
            result
        else
          messagedReducer entity
      )
    |> Maybe.withDefault entity
