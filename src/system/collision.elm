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

type alias Manifold = ((Component.Spatial.Model, Component.Collidable.Model), (Component.Spatial.Model, Component.Collidable.Model))

detect : ((Entity.Role.Name, Entity.Model) -> (Entity.Role.Name, Entity.Model) -> Cmd msg) -> (Entity.Model, Entity.Model) -> Maybe (Cmd msg)
detect interactor (self, other) =
  Entity.getCollidablePair (self, other)
    `andThen` batchInteractions interactor (self, other)

batchInteractions : ((Entity.Role.Name, Entity.Model) -> (Entity.Role.Name, Entity.Model) -> Cmd msg) -> (Entity.Model, Entity.Model) -> (Component.Collidable.Model, Component.Collidable.Model) -> Maybe (Cmd msg)
batchInteractions interactor (self, other) (selfColl, otherColl) =
  case touching self other of
    True ->
      Just <| Cmd.batch [
        interactor (selfColl.role, self) (otherColl.role, other)
      , interactor (otherColl.role, other) (selfColl.role, self)
      ]
    _ ->
      Nothing

collide : Entity.Model -> Entity.Model -> Entity.Model
collide self other =
  Maybe.map2 (,)
    (Maybe.map2 (,) (Entity.getSpatial self) (Entity.getCollidable self))
    (Maybe.map2 (,) (Entity.getSpatial other) (Entity.getCollidable other))
  |> Maybe.map (collisionAlgo self)
  |> Maybe.withDefault self

collisionAlgo : Entity.Model -> ((Component.Spatial.Model, Component.Collidable.Model), (Component.Spatial.Model, Component.Collidable.Model)) -> Entity.Model
collisionAlgo self ((selfSpace, selfColl), (otherSpace, otherColl)) =
  let
    normal = otherSpace.pos |- selfSpace.pos
    rv = otherSpace.vel |- selfSpace.vel
    normalVel = dot rv normal
  in
    if normalVel <= 0 then
      let
        restitution = min selfColl.restitution otherColl.restitution
        impulseScalar = -(1 + restitution) * normalVel / (1 / selfSpace.mass + 1 / otherSpace.mass)
        impulse = normal .* impulseScalar
      in
        Entity.filterMapSpatial (\space ->
          { space | vel = space.vel |+ (impulse ./ selfSpace.mass) }
        ) self
    else
      self

--collisionAlgo1 : Entity.Model -> Manifold -> Entity.Model
--collisionAlgo1 self ((selfSpace, selfColl), (otherSpace, otherColl)) =
--  let
--    cor = selfColl.restitution
--    m1 = selfSpace.mass
--    v1 = selfSpace.vel
--    x1 = selfSpace.pos
--    m2 = otherSpace.mass
--    v2 = otherSpace.vel
--    x2 = otherSpace.pos
--
--    n = x2 |- x1
--    un = n ./ (norm n)
--    ut = (-(Vec.y un), Vec.x un)
--    v1n = Vec.dot un v1
--    v1t = Vec.dot ut v1
--    v2n = Vec.dot un v2
--    v1n' = un .* ((v1n * (m1 - m2) + 2 * m2 * v2n) / (m1 + m2))
--    v1t' = ut .* v1t
--  in
--    self
--    |> Entity.filterMapSpatial (\space ->
--        { space |
--          --vel = ((x1 |- x2) .* (2 * m2 / m1 + m2)) .* ((dot (v1 |- v2) (x1 |- x2)) / normSqr (x1 |- x2))
--          --vel = (-(fst space.vel), -(snd space.vel))
--          vel = v1n' |+ v1t'
--        } )


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
