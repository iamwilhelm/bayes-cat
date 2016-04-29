module Collision where

import Entity exposing (..)
import Component
import Vec exposing (..)

import Signal
import Task
import Effects exposing (Effects)
import Action exposing (Action, EntityAction)

type alias Range = (Float, Float)

between : Range -> Range -> Bool
between (minA, maxA) (minB, maxB) =
  if (minA < minB && minA < maxB) && (maxA < minB && maxA < maxB) then
    False
  else if (minA > minB && minA > maxB) && (maxA > minB && maxA > maxB) then
    False
  else
    True

inside : Entity -> Entity -> Bool
inside self othr =
  let
    selfMin = self.space.pos |- self.corp.dim ./ 2
    selfMax = self.space.pos |+ self.corp.dim ./ 2
    othrMin = othr.space.pos |- othr.corp.dim ./ 2
    othrMax = othr.space.pos |+ othr.corp.dim ./ 2
  in
    if between (Vec.x selfMin, Vec.x selfMax) (Vec.x othrMin, Vec.x othrMax)
      && between (Vec.y selfMin, Vec.y selfMax) (Vec.y othrMin, Vec.y othrMax) then
      True
    else
      False

collide : Entity -> Entity -> Entity
collide self other =
  { self |
    space = Component.setVel (Vec.neg self.space.vel) self.space
  }

interact : Signal.Address (List Action) -> Entity -> Entity -> Effects Action
interact address self other =
  let
    _ = Debug.log "in interact--self : " self.role
    a = Debug.log "in interact--other: " other.role
  in
    if inside self other then
      case (self.role, other.role) of
        (Egg, Cursor) ->
          Effects.task <| Task.succeed (Action.Entity Action.Open)
        _ ->
          Effects.none
    else
      Effects.none

------------- pairing algorithms

squaredUpdate : (Entity -> Entity -> Effects Action) -> List Entity -> List (Effects Action)
squaredUpdate interactionCallback entities =
  let
    everyOtherEntities index entities =
      List.append (List.take index entities) (List.drop (index + 1) entities)

    gatherEffects interactionCallback entity otherEntities =
      List.foldl (\other effects ->
        (interactionCallback entity other) :: effects
      ) [] otherEntities
  in
    List.concat
    <| List.indexedMap (\index entity ->
         gatherEffects interactionCallback entity (everyOtherEntities index entities)
       ) entities
