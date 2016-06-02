module System.Renderer exposing (..)

import Maybe exposing (andThen)
import Collage
import Transform exposing (Transform, multiply)

import Router

import Entity

import Component.Spatial
import Component.Renderable

render : Maybe Entity.Model -> Entity.Model -> Maybe Collage.Form
render camera entity =
  let
    entityForms = Entity.getRenderable entity `andThen` Router.renderEntity entity
    tf = totalTransform camera entity
  in
    Maybe.map (Collage.groupTransform tf) entityForms


totalTransform : Maybe Entity.Model -> Entity.Model -> Transform
totalTransform camera entity =
  (e2w entity) `multiply` (w2c camera)

e2w : Entity.Model -> Transform
e2w entity =
  Entity.getSpatial entity
  |> Maybe.map Component.Spatial.transform
  |> Maybe.withDefault Transform.identity

w2c : Maybe Entity.Model -> Transform
w2c camera =
  camera `andThen` Entity.getSpatial
  |> Maybe.map Component.Spatial.invertedControlTransform
  |> Maybe.withDefault Transform.identity
