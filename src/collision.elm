module Collision where

import Entity exposing (..)
import Entity.EntityList
import Entity.Egg exposing (Egg)
import Component
import Vec exposing (..)

import Signal
import Task
import Effects exposing (Effects)
import Action exposing (Action)
import Role

type alias Range = (Float, Float)

overlap : Range -> Range -> Bool
overlap (minA, maxA) (minB, maxB) =
  if (minA < minB && minA < maxB) && (maxA < minB && maxA < maxB) then
    False
  else if (minA > minB && minA > maxB) && (maxA > minB && maxA > maxB) then
    False
  else
    True

withinRange : Range -> Range -> Bool
withinRange (minA, maxA) (minB, maxB) =
  if (minA > minB && minA < maxB) && (maxA > minB && maxA < maxB) then
    True
  else
    False

inside : Entity -> Entity -> Bool
inside self othr =
  let
    selfMin = self.space.pos |- self.corp.dim ./ 2
    selfMax = self.space.pos |+ self.corp.dim ./ 2
    othrMin = othr.space.pos |- othr.corp.dim ./ 2
    othrMax = othr.space.pos |+ othr.corp.dim ./ 2
  in
    overlap (Vec.x selfMin, Vec.x selfMax) (Vec.x othrMin, Vec.x othrMax)
    && overlap (Vec.y selfMin, Vec.y selfMax) (Vec.y othrMin, Vec.y othrMax)

withinBounds : (Float, Float, Float, Float) -> Entity -> Bool
withinBounds (top, right, bottom, left) self =
  let
    selfMin = self.space.pos |- self.corp.dim ./ 2
    selfMax = self.space.pos |+ self.corp.dim ./ 2
  in
    withinRange (Vec.x selfMin, Vec.x selfMax) (left, right)
    && withinRange (Vec.y selfMin, Vec.y selfMax) (bottom, top)

collide : Entity -> Entity -> Entity
collide self other =
  { self |
    space = Component.setVel (Vec.neg self.space.vel) self.space
  }

interact : Signal.Address (List Action) -> (Int, Entity) -> (Int, Entity) -> Effects Action
interact address (selfId, self) (otherId, other) =
  if inside self other then
    let
      _ = Debug.log "self vs other" (self.role, other.role)
    in
      case (self.role, other.role) of
        (Role.Pointer, Role.Egg) ->
          Effects.task
            <| Task.succeed (Action.EntityList Entity.EntityList.Update selfID Entity.Egg.Open)
        _ ->
          Effects.none
  else
    Effects.none

-- private
gatherEffects : ((Entity.EntityList.ID, Entity.Egg.Egg) -> (Entity.EntityList.ID, Entity.Egg.Egg) -> Effects Action) -> (Entity.EntityList.ID, Entity.Egg.Egg) -> List (Entity.EntityList.ID, Entity.Egg.Egg) -> List (Effects Action)
gatherEffects interactionCallback entityPair otherEntityPairs =
  List.foldl (\otherPair effects ->
    List.filter (\effect -> effect /= Effects.none)
      <| (interactionCallback entityPair otherPair) :: effects
  ) [] otherEntityPairs
