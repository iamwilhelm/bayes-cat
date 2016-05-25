module System.Collision exposing (..)

import Vec exposing (..)

import Entity
import Entity.Role
import Entity.Cat
import Entity.Egg

import Component
import Component.Spatial

type alias Range = (Float, Float)

detect : ((Entity.Model, Entity.Model) -> Maybe (Cmd msg)) -> (Entity.Model, Entity.Model) -> Maybe (Cmd msg)
detect router (self, other) =
  if touching self other then
    let
      result = router (self, other)
      _ = Debug.log "touching" result
    in
      result
  else
    Nothing

collide : Entity.Model -> Entity.Model -> Entity.Model
collide self other =
  Entity.filterMapSpatial (\space ->
    Component.Spatial.vel (Vec.neg space.vel) space
  ) self

-- helper functions

overlap : Range -> Range -> Bool
overlap (minA, maxA) (minB, maxB) =
  if (minA < minB && minA < maxB) && (maxA < minB && maxA < maxB) then
    False
  else if (minA > minB && minA > maxB) && (maxA > minB && maxA > maxB) then
    False
  else
    True

touching : Entity.Model -> Entity.Model -> Bool
touching self other =
  let
    mSelfSpace = Entity.getSpatial self
    mSelfCorp = Entity.getCorporeal self
    mOtherSpace = Entity.getSpatial other
    mOtherCorp = Entity.getCorporeal other
  in
    case (mSelfSpace, mSelfCorp, mOtherSpace, mOtherCorp) of
      (Just selfSpace, Just selfCorp, Just otherSpace, Just otherCorp) ->
        let
          selfMin = selfSpace.pos |- selfCorp.dim ./ 2
          selfMax = selfSpace.pos |+ selfCorp.dim ./ 2
          otherMin = otherSpace.pos |- otherCorp.dim ./ 2
          otherMax = otherSpace.pos |+ otherCorp.dim ./ 2
        in
          overlap (Vec.x selfMin, Vec.x selfMax) (Vec.x otherMin, Vec.x otherMax)
          && overlap (Vec.y selfMin, Vec.y selfMax) (Vec.y otherMin, Vec.y otherMax)
      _ ->
        False

--within : Range -> Range -> Bool
--within (minA, maxA) (minB, maxB) =
--  if (minA > minB && minA < maxB) && (maxA > minB && maxA < maxB) then
--    True
--  else
--    False
--
--inside : (Float, Float, Float, Float) -> Entity.Model -> Bool
--inside (top, right, bottom, left) self =
--  let
--    selfMin = self.space.pos |- self.corp.dim ./ 2
--    selfMax = self.space.pos |+ self.corp.dim ./ 2
--  in
--    within (Vec.x selfMin, Vec.x selfMax) (left, right)
--    && within (Vec.y selfMin, Vec.y selfMax) (bottom, top)
--
