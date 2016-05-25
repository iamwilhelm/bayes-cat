module Entity.Egg exposing (..)

import Collage exposing (..)
import Color

import Entity
import Entity.Role
import Component
import Component.Corporeal
import Component.Gravitate
import Vec exposing (..)

import Debug

-- model

init : Entity.ID -> Entity.Model
init id = {
    id = id
  , components = [
      Entity.spatial 10 (0, 0)
    , Entity.corporeal (35, 35) Color.gray
    , Entity.gravitate Component.Gravitate.ToMoon
    , Entity.viewable Entity.Role.Egg
    , Entity.collidable Entity.Role.Egg
    ]
  }

-- update

type alias Id = Int

type Msg =
    Open Id
  | Close Id
  | NoOp

reduce : Msg -> Entity.Model -> Entity.Model
reduce action model =
  case action of
    Open id ->
      if model.id == id then
        Entity.filterMapCorporeal (Component.Corporeal.color Color.blue) model
      else
        model
    Close id ->
      if model.id == id then
        Entity.filterMapCorporeal (Component.Corporeal.color Color.gray) model
      else
        model
    NoOp ->
      model

-- interaction

-- what will other entities do to egg?
interact : (Entity.Role.Name, Entity.Model) -> (Entity.Role.Name, Entity.Model) -> Cmd Msg
interact (selfRole, self) (otherRole, other) =
  case otherRole of
    Entity.Role.Cat ->
      Cmd.map (\_ -> Open self.id) Cmd.none
    Entity.Role.Egg ->
      Cmd.map (\_ -> Close self.id) Cmd.none

-- view

view : Entity.Model -> Maybe Form
view entity =
  let
    maybeSpace = Component.getSpatial entity.components
    maybeCorp = Component.getCorporeal entity.components
  in
    case (maybeSpace, maybeCorp) of
      (Just space, Just corp) ->
        Just
        <| move space.pos
          <| group [
            filled corp.color <| circle ((fst corp.dim) / 2)
          ]
      _ ->
        Nothing
