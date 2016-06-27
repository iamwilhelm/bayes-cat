module Entity exposing (..)

import Window
import Collage exposing (..)
import Color

import Entity.Role
import Component
import Component.Spatial
import Component.Corporeal
import Component.Label
import Component.Gravitate
import Component.Renderable
import Component.Controllable
import Component.Collidable

import Transform exposing (Transform, multiply)

import Vec exposing (..)
import Debug

type alias ID = Int

type alias Model = {
    id : ID
  , components : List Component.Model
  }

-- accessors

getSpatial : Model -> Maybe Component.Spatial.Model
getSpatial model =
  Component.getSpatial model.components

getCorporeal : Model -> Maybe Component.Corporeal.Model
getCorporeal model =
  Component.getCorporeal model.components

getGravitate : Model -> Maybe Component.Gravitate.Model
getGravitate model =
  Component.getGravitate model.components

getRenderable : Model -> Maybe Component.Renderable.Model
getRenderable model =
  Component.getRenderable model.components

getControllable : Model -> Maybe Component.Controllable.Model
getControllable model =
  Component.getControllable model.components

getCollidable : Model -> Maybe Component.Collidable.Model
getCollidable model =
  Component.getCollidable model.components

getCollidablePair : (Model, Model) -> Maybe (Component.Collidable.Model, Component.Collidable.Model)
getCollidablePair (self, other) =
  case (Component.getCollidable self.components, Component.getCollidable other.components) of
    (Just selfColl, Just otherColl) ->
      Just (selfColl, otherColl)
    _ ->
      Nothing

filterMapSpatial : (Component.Spatial.Model -> Component.Spatial.Model) -> Model -> Model
filterMapSpatial func model =
  { model | components = Component.filterMapSpatial func model.components }

filterMapCorporeal : (Component.Corporeal.Model -> Component.Corporeal.Model) -> Model -> Model
filterMapCorporeal func model =
  { model | components = Component.filterMapCorporeal func model.components }

filterMapCollidable : (Component.Collidable.Model -> Component.Collidable.Model) -> Model -> Model
filterMapCollidable func model =
  { model | components = Component.filterMapCollidable func model.components }

-- accessors

applyImpulse : Vec -> Model -> Model
applyImpulse impulse model =
  filterMapSpatial (\space ->
    let
      _ = Debug.log "impulse" impulse
    in
      { space | vel = space.vel |+ impulse }
  ) model

-- system calls

boundFloor : Window.Size -> Model -> Model
boundFloor size model =
  filterMapSpatial (\space ->
    let
      (w', h') = (size.width, size.height)
      (w, h) = (toFloat w', toFloat h')
    in
      if Vec.y space.pos < -(h / 2) then
        space
        |> Component.Spatial.vel ((Vec.x space.vel, -(Vec.y space.vel)) .* 0.9)
        |> Component.Spatial.pos (Vec.x space.pos, -(h / 2))
      else
        space
  ) model

boundWalls : Window.Size -> Model -> Model
boundWalls size model =
  filterMapSpatial (\space ->
    let
      (w', h') = (size.width, size.height)
      (w, h) = (toFloat w', toFloat h')
    in
      if Vec.x space.pos < -(w / 2) then
        space
        |> Component.Spatial.vel (-(Vec.x space.vel), Vec.y space.vel)
        |> Component.Spatial.pos (-(w / 2), Vec.y space.pos)
      else
        if Vec.x space.pos > (w / 2) then
          space
          |> Component.Spatial.vel (-(Vec.x space.vel), Vec.y space.vel)
          |> Component.Spatial.pos ((w / 2), Vec.y space.pos)
        else
          space
  ) model
