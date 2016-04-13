module Collision where

import Entity exposing (..)
import Vec

type alias Range = (Float, Float)

between : Range -> Range -> Bool
between (minA, maxA) (minB, maxB) =
  -- checks
  if (minA < minB && minA < maxB) && (maxA < minB && maxA < maxB) then
    False
  else if (minA > minB && minA > maxB) && (maxA > minB && maxA > maxB) then
    False
  else
    True

inside : Entity -> Entity -> Bool
inside self othr =
  let
    selfMin = Vec.sub self.space.pos <| Vec.divS self.corp.dim 2
    selfMax = Vec.add self.space.pos <| Vec.divS self.corp.dim 2
    othrMin = Vec.sub othr.space.pos <| Vec.divS othr.corp.dim 2
    othrMax = Vec.add othr.space.pos <| Vec.divS othr.corp.dim 2
  in
    if between (Vec.x selfMin, Vec.x selfMax) (Vec.x othrMin, Vec.x othrMax)
      && between (Vec.y selfMin, Vec.y selfMax) (Vec.y othrMin, Vec.y othrMax) then
      True
    else
      False

collide : Entity -> Entity -> Entity
collide other self =
  if flip inside self other then
    -- run through all interactions of self and update self
    List.foldl (\interaction entity ->
      Entity.route interaction entity other
    ) self self.interactions

    -- run through all interactions of other and update other
  else
    self

------------- pairing algorithms

-- interactionCallback : other -> target -> target
pairwiseUpdate : (Entity -> Entity -> Entity) -> List Entity -> List Entity
pairwiseUpdate interactionCallback entities =
  if List.length entities <= 1 then
    entities
  else
    List.indexedMap (\index entity ->
      if index == 0 then
        entity
      else
        List.foldl interactionCallback entity
        <| List.drop (index + 1) entities
    ) entities

squaredUpdate : (Entity -> Entity -> Entity) -> List Entity -> List Entity
squaredUpdate interactionCallback entities =
  List.indexedMap (\index entity ->
    List.foldl (\other self ->
      interactionCallback other self
    ) entity <| List.append (List.take index entities) (List.drop (index + 1) entities)
  ) entities
