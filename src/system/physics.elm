module System.Physics exposing (..)

import Entity
import Component.Spatial

import Vec exposing (..)

gravity : Float -> Entity.Model -> Entity.Model
gravity dt entity =
  let
    maybeGrav = Entity.getGravitate entity
  in
    case maybeGrav of
      Just grav ->
        Entity.filterMapSpatial (\space ->
          Component.Spatial.acc (grav.acc .* dt) space
        ) entity
      Nothing ->
        entity

{-| Apply newtonian physics to the entity.

Uses the Velocity Verlet integration to calculate the physics
-}
newtonian : Float -> Entity.Model -> Entity.Model
newtonian dt entity =
  Entity.filterMapSpatial (\space ->
    let
      acc = Component.Spatial.totalAcc space
      space2 = Component.Spatial.vel (space.vel |+ acc .* (dt / 10)) space
    in
      Component.Spatial.pos (space2.pos |+ ((space.vel |+ space2.vel) .* (0.5 * (dt / 10)))) space2
  ) entity
