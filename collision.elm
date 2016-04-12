module Collision where

import Entity exposing (..)
import Vec

inside : Entity -> Entity -> Bool
inside self other =
  let
    selfPos = self.space.pos
    otherPos = other.space.pos
    selfDim = self.corp.dim
    otherDim = other.corp.dim
  in
    if  (Vec.y otherPos > (Vec.y selfPos - Vec.y selfDim / 2)
      && Vec.y otherPos < (Vec.y selfPos + Vec.y selfDim / 2)
      && Vec.x otherPos > (Vec.x selfPos - Vec.x selfDim / 2)
      && Vec.x otherPos < (Vec.x selfPos + Vec.x selfDim / 2)) then
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
