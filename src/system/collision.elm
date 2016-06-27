module System.Collision exposing (
  detect, touching, overlap, dist, impulseMsg, Manifold)

import Maybe exposing (andThen)
import Task

import Vec exposing (..)

import Entity
import Entity.Role

import Component
import Component.Spatial
import Component.Corporeal
import Component.Collidable

type alias Range = (Float, Float)

type alias Manifold = {
    entity : Entity.Model
  , space : Component.Spatial.Model
  , corp : Component.Corporeal.Model
  , coll : Component.Collidable.Model
  }

type alias Interaction = (Manifold, Manifold)

detect : (Manifold -> Manifold -> Cmd msg) -> (Entity.Model, Entity.Model) -> Maybe (Cmd msg)
detect interactor (self, other) =
  createManifoldPair self other
    `andThen` collisionInteractionMsg interactor

createManifoldPair : Entity.Model -> Entity.Model -> Maybe (Manifold, Manifold)
createManifoldPair self other =
  Maybe.map2 (,) (initManifold self) (initManifold other)

initManifold : Entity.Model -> Maybe Manifold
initManifold entity =
  Maybe.map3 (,,) (Entity.getSpatial entity) (Entity.getCorporeal entity) (Entity.getCollidable entity)
  |> Maybe.map (\(space, corp, coll) ->
      { entity = entity , space = space , corp = corp, coll = coll }
    )

collisionInteractionMsg : (Manifold -> Manifold -> Cmd msg) -> (Manifold, Manifold) -> Maybe (Cmd msg)
collisionInteractionMsg interactor (selfM, otherM) =
  if not (touching selfM otherM) then
    Nothing
  else
    Just <| interactor selfM otherM

impulseMsg : (Vec -> Vec -> Cmd msg) -> Manifold -> Manifold -> Cmd msg
impulseMsg msg self other =
  let
    -- FIXME WIP. The physics of the collision isn't working correctly
    restitution = min self.coll.restitution other.coll.restitution
    m1 = self.space.mass
    v1 = self.space.vel
    x1 = self.space.pos
    m2 = other.space.mass
    v2 = other.space.vel
    x2 = other.space.pos

    n = x2 |- x1
    un = n ./ (norm n)
    ut = Vec.orthogonal un
    v = v2 |- v1
    vn = Vec.dot un v
    v1n = Vec.dot un v1
    v1t = Vec.dot ut v1
    v2n = Vec.dot un v2
    v2t = Vec.dot ut v2
    v1n' = un .* ((v1n * (m1 - m2) + 2 * m2 * v2n) / (m1 + m2))
    v1t' = ut .* v1t
    v2n' = un .* ((v2n * (m2 - m1) + 2 * m1 * v1n) / (m2 + m1))
    v2t' = ut .* v2t
    v1' = v1n' |+ v1t'
    v2' = v2n' |+ v2t'
  in
    msg v1 (Vec.neg v2)

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

touching : Manifold -> Manifold-> Bool
touching self other =
  let
    selfMin = self.space.pos |- self.corp.dim ./ 2
    selfMax = self.space.pos |+ self.corp.dim ./ 2
    otherMin = other.space.pos |- other.corp.dim ./ 2
    otherMax = other.space.pos |+ other.corp.dim ./ 2
  in
    overlap (Vec.x selfMin, Vec.x selfMax) (Vec.x otherMin, Vec.x otherMax)
    && overlap (Vec.y selfMin, Vec.y selfMax) (Vec.y otherMin, Vec.y otherMax)

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
