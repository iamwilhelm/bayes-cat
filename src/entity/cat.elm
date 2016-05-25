module Entity.Cat exposing (..)

import Collage exposing (..)
import Color

import Entity
import Entity.Role
import Component
import Component.Spatial
import Component.Corporeal
import Component.Gravitate
import Vec exposing (..)

import Debug

-- model

init : Entity.ID -> Entity.Model
init id = {
    id = id
  , components = [
      Entity.spatial 50 (100, 0)
    , Entity.corporeal (45, 45) Color.orange
    , Entity.gravitate Component.Gravitate.ToEarth
    , Entity.controllable Entity.Role.Cat
    , Entity.viewable Entity.Role.Cat
    , Entity.collidable Entity.Role.Cat
    ]
  }

-- update

type Msg =
    Move MsgDirection
  | Grow
  | NoOp

type MsgDirection =
    Up
  | Down
  | Left
  | Right

reduce : Msg -> Entity.Model -> Entity.Model
reduce msg model =
  case msg of
    Move direction ->
      reduceMove direction model
    Grow ->
      Entity.filterMapCorporeal (Component.Corporeal.grow 5) model
    NoOp ->
      model

reduceMove : MsgDirection -> Entity.Model -> Entity.Model
reduceMove direction model =
  case direction of
    Up ->
      Entity.filterMapSpatial (Component.Spatial.insertForce (0, 100)) model
    Down ->
      Entity.filterMapSpatial (Component.Spatial.insertForce (0, -150)) model
    Left ->
      Entity.filterMapSpatial (Component.Spatial.insertForce (-50, 0)) model
    Right ->
      Entity.filterMapSpatial (Component.Spatial.insertForce (50, 0)) model

-- interaction

-- what will other entities do to cat?
interact : (Entity.Role.Name, Entity.Model) -> (Entity.Role.Name, Entity.Model) -> Cmd Msg
interact (selfRole, self) (otherRole, other) =
  let
    result = case otherRole of
      Entity.Role.Egg ->
        let
          result = Cmd.map (\_ -> Grow) Cmd.none
          _ = Debug.log "cat egg" result
        in
          result
      _ ->
        Cmd.map (\_ -> NoOp) Cmd.none
    _ = Debug.log "interact"
  in
    result

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
            , viewLeftEar corp
            , viewRightEar corp
            ]
      _ ->
        Nothing

viewLeftEar : Component.Corporeal.Model -> Form
viewLeftEar corp =
  move ((fst corp.dim) / 2 * cos (degrees 135), ((fst corp.dim) / 2 * sin (degrees 45)))
    <| filled corp.color <| ngon 3 ((fst corp.dim) / 4)

viewRightEar : Component.Corporeal.Model -> Form
viewRightEar corp =
  move ((fst corp.dim) / 2 * cos (degrees 45), ((fst corp.dim) / 2 * sin (degrees 45)))
    <| rotate (degrees 180)
    <| filled corp.color <| ngon 3 ((fst corp.dim) / 4)
