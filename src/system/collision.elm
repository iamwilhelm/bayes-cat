module System.Collision exposing (..)

import Maybe exposing (andThen)

import Vec exposing (..)

import Entity
import Entity.Role

import Component
import Component.Spatial
import Component.Collidable

type alias Range = (Float, Float)

detect : ((Entity.Role.Name, Entity.Model) -> (Entity.Role.Name, Entity.Model) -> Cmd msg) -> (Entity.Model, Entity.Model) -> Maybe (Cmd msg)
detect interactor (self, other) =
  if touching self other then
    Entity.getCollidablePair (self, other) `andThen`
      batchInteractions interactor (self, other)
  else
    Nothing

batchInteractions : ((Entity.Role.Name, Entity.Model) -> (Entity.Role.Name, Entity.Model) -> Cmd msg) -> (Entity.Model, Entity.Model) -> (Component.Collidable.Model, Component.Collidable.Model) -> Maybe (Cmd msg)
batchInteractions interactor (self, other) (selfColl, otherColl) =
  Just <| Cmd.batch [
    interactor (selfColl.role, self) (otherColl.role, other)
  , interactor (otherColl.role, other) (selfColl.role, self)
  ]

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
