module System.Collision exposing (detect, touching, overlap, collide, dist)

import Maybe exposing (andThen)
import Task

import Vec exposing (..)

import Entity
import Entity.Role

import Component
import Component.Spatial
import Component.Collidable

type alias Range = (Float, Float)

detect : ((Entity.Role.Name, Bool, Entity.Model) -> (Entity.Role.Name, Bool, Entity.Model) -> Cmd msg) -> (Entity.Model, Entity.Model) -> Maybe (Cmd msg)
detect interactor (self, other) =
  Entity.getCollidablePair (self, other)
    `andThen` batchInteractions interactor (self, other)

batchInteractions : ((Entity.Role.Name, Bool, Entity.Model) -> (Entity.Role.Name, Bool, Entity.Model) -> Cmd msg) -> (Entity.Model, Entity.Model) -> (Component.Collidable.Model, Component.Collidable.Model) -> Maybe (Cmd msg)
batchInteractions interactor (self, other) (selfColl, otherColl) =
  case (touching self other, selfColl.isColliding, otherColl.isColliding) of
    (True, False, False) ->
      Just <| Cmd.batch [
        interactor (selfColl.role, False, self) (otherColl.role, False, other)
      , interactor (otherColl.role, False, other) (selfColl.role, False, self)
      ]
    (False, True, True) ->
      Just <| Cmd.batch [
      ]
    _ ->
      Nothing

collide : Entity.Model -> Entity.Model -> Entity.Model
collide self other =
  Maybe.map2 (,)
    (Maybe.map2 (,) (Entity.getSpatial self) (Entity.getCollidable self))
    (Maybe.map2 (,) (Entity.getSpatial other) (Entity.getCollidable other))
  |> Maybe.map (\((selfSpace, selfColl), (otherSpace, otherColl)) ->
      let
        cor = selfColl.restitution
        m1 = selfSpace.mass
        v1 = selfSpace.vel
        x1 = selfSpace.pos
        m2 = otherSpace.mass
        v2 = otherSpace.vel
        x2 = otherSpace.pos
      in
        self
        |> Entity.filterMapSpatial (\space ->
            { space |
              -- vel = (x1 |- x2) .* ((2 * m2 / m1 + m2) * ((dot (v1 |- v2) (x1 |- x2)) / normSqr (x1 |- x2)))
              vel = (fst space.vel, -(snd space.vel))
            } )
    )
  |> Maybe.withDefault self

dist : Entity.Model -> Entity.Model -> Float
dist self other =
  Maybe.map2 (,) (Entity.getSpatial self) (Entity.getSpatial other)
  |> Maybe.map (\(s1, s2) -> Vec.norm (s1.pos |- s2.pos) )
  |> Maybe.withDefault -1.0

--calcCollision : (Component.Spatial.Model, Component.Collidable.Model) -> (C)

-- helper functions
-- TODO should these be in a library called geometry? (but they can't refer to entity)

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
  -- TODO replace with Maybe.map4
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
