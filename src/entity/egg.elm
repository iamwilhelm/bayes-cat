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

-- FIXME should entities be able to reference systems?
import System.Collision

import Debug

-- model

init : Entity.ID -> Vec -> Vec -> Entity.Model
init id pos vel = {
    id = id
  , components = [
      Component.spatial 20 pos vel
    , Component.corporeal (35, 35) Color.gray
    , Component.gravitate Component.Gravitate.ToEarth
    , Component.renderable Entity.Role.Egg
    , Component.collidable Entity.Role.Egg 1 10
    ]
  }

-- update

type alias Id = Int

type Msg =
    Open Id
  | Close Id
  | Bounce Id Entity.Model
  | Collide Id CollideMsg
  | NoOp

type CollideMsg =
    Enter
  | Exit

reduce : Msg -> Entity.Model -> Entity.Model
reduce msg model =
  case msg of
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
    Bounce id other ->
      if model.id == id then
        System.Collision.collide model other
      else
        model
    Collide id msg ->
      if model.id == id then
        reduceCollide msg model
      else
        model
    _ ->
      model

reduceCollide : CollideMsg -> Entity.Model -> Entity.Model
reduceCollide msg model =
  case msg of
    Enter ->
      Entity.filterMapCollidable (\coll -> { coll | isColliding = True })
        <| Entity.filterMapCorporeal (Component.Corporeal.color Color.red)
        <| model
    Exit ->
      Entity.filterMapCollidable (\coll -> { coll | isColliding = False })
        <| Entity.filterMapCorporeal (Component.Corporeal.color Color.green)
        <| model

-- interaction

-- what will other entities do to egg?
interact : (Entity.Role.Name, Entity.Model) -> (Entity.Role.Name, Entity.Model) -> Cmd Msg
interact (selfRole, self) (otherRole, other) =
  case otherRole of
    Entity.Role.Cat ->
      --Task.perform never identity (Task.succeed (Open self.id))
      Task.perform never identity (Task.succeed NoOp)
    Entity.Role.Egg ->
      let
        _ = 3 --Debug.log "egg on egg" (self.id, other.id)
        --a = Debug.log "dist" <| System.Collision.dist self other
        --b = Debug.log "vel" <| (self.id, Maybe.withDefault (-1.0, -1.0) <| Maybe.map (\s -> s.vel) <| Entity.getSpatial self)
      in
        Cmd.batch [
          Task.perform never identity (Task.succeed (Collide self.id Enter))
        , Task.perform never identity (Task.succeed (Bounce self.id other))
        ]
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
