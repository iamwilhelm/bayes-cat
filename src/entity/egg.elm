module Entity.Egg exposing (..)

import Basics.Extra exposing (never)
import Collage exposing (..)
import Task
import Color

import Entity
import Entity.Role
import Component
import Component.Corporeal
import Component.Gravitate
import Vec exposing (..)

import Debug

-- model

init : Entity.ID -> Vec -> Vec -> Entity.Model
init id pos vel = {
    id = id
  , components = [
      Component.spatial 20 pos vel
    , Component.corporeal (35, 35) Color.gray
    , Component.gravitate Component.Gravitate.ToMoon
    , Component.renderable Entity.Role.Egg
    , Component.collidable Entity.Role.Egg 1 10
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
    _ ->
      model

-- interaction

-- what will other entities do to egg?
interact : (Entity.Role.Name, Entity.Model) -> (Entity.Role.Name, Entity.Model) -> Cmd Msg
interact (selfRole, self) (otherRole, other) =
  case otherRole of
    Entity.Role.Cat ->
      Task.perform never identity (Task.succeed (Open self.id))
    Entity.Role.Egg ->
      Task.perform never identity (Task.succeed (Close self.id))
    _ ->
      Task.perform never identity (Task.succeed NoOp)


-- view

view : Entity.Model -> Maybe (List Form)
view entity =
  Entity.getCorporeal entity
  |> Maybe.map (\corp ->
    [
      filled corp.color <| circle ((fst corp.dim) / 2)
    ]
  )
