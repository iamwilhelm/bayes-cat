module System.Physics exposing (..)

import Entity
import Component.Spatial

import Vec exposing (..)

{-| Apply a constant acceleration of gravity to entity
-}
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

TODO need to cap limits of velocity before translating to position

-}
newtonian : Float -> Entity.Model -> Entity.Model
newtonian dt entity =
  Entity.filterMapSpatial (\space ->
    let
      space2 = Component.Spatial.acc (Component.Spatial.totalAcc space) space
      space3 = Component.Spatial.acc (Vec.clamp (fst space.velLimit) (snd space.velLimit) space2.acc) space2

      space4 = Component.Spatial.vel (space.vel |+ ((space.acc |+ space3.acc) .* (0.5 * (dt / 10)))) space3
      space5 = Component.Spatial.vel (Vec.clamp (fst space.accLimit) (snd space.accLimit) space4.vel) space4

      space6 = Component.Spatial.pos (space5.pos |+ ((space.vel |+ space5.vel) .* (0.5 * (dt / 10)))) space5
    in
      space6
  ) entity

{-| Clears the forces in the entity, so we can calculate them from forces anew.
You'll want to compose this at the end of all system calls
-}
clearForces : Entity.Model -> Entity.Model
clearForces entity =
  Entity.filterMapSpatial Component.Spatial.clearForces entity
