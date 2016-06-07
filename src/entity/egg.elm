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
    , Component.collidable Entity.Role.Egg 1
    ]
  }

-- update

type alias Id = Int

type Msg =
    Open Id
  | Close Id
  | Impulse Id Vec
  | NoOp

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
    Impulse id impulse ->
      if model.id == id then
        Entity.applyImpulse impulse model
      else
        model
    _ ->
      model

-- interaction

-- what will other entities do to egg?
interact : System.Collision.Manifold -> System.Collision.Manifold -> Cmd Msg
interact selfM otherM =
  case otherM.coll.role of
    Entity.Role.Cat ->
      --Task.perform never identity (Task.succeed (Open self.id))
      Task.perform never identity (Task.succeed NoOp)
    Entity.Role.Egg ->
      Task.perform never identity (Task.succeed NoOp)
      --System.Collision.impulseMsg (\selfImpulse otherImpulse ->
      --  Cmd.batch [
      --    Task.perform never identity (Task.succeed <| Impulse selfM.entity.id selfImpulse)
      --  , Task.perform never identity (Task.succeed <| Impulse otherM.entity.id otherImpulse)
      --  ]
      --) selfM otherM
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
