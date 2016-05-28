import Window
import AnimationFrame
import Mouse
import Random
import Keyboard
import Task
import Char

import Time exposing (Time)
import List.Extra exposing (pairs)

import Collage
import Element exposing (..)
import Color exposing (Color)
import Maybe exposing (andThen)
import Text

import Html exposing (..)
import Html.App as App

import Entity
import Entity.Role
import Entity.Egg
import Entity.Cat

import System.Physics
import System.Control
import System.View
import System.Collision
--import Viewport

import Debug

-------------- Model methods

type alias Model =
  { entities : List Entity.Model
  , nextEntityId : Int
  , seed : Random.Seed
  , size : Window.Size
  }

init : (Model, Cmd Msg)
init =
  ({
      entities = [
        Entity.Cat.init 0
      , Entity.Egg.init 1 (-300, 100) (10, 0)
      , Entity.Egg.init 2 (-200, 100) (5, 10)
      , Entity.Egg.init 3 (-100, 100) (-10, -10)
      , Entity.Egg.init 4 (100, 100) (10, -5)
      , Entity.Egg.init 5 (200, 100) (2, 5)
      , Entity.Egg.init 6 (300, 100) (-4, 2)
      ]
    , nextEntityId = 7
    , seed = Random.initialSeed 0
    , size = Window.Size 0 0
    }
  , Task.perform (\_ -> NoOp) SizeChange Window.size
  )

map : (Entity.Model -> Entity.Model) -> Model -> Model
map func model =
  { model | entities = List.map func model.entities }

foldl : (Entity.Model -> b -> b) -> b -> Model -> b
foldl func acc model =
  List.foldl func acc model.entities


-------------- Update methods

-- TODO may have to put this in a module, because entity instances
-- need to express effects
type Msg =
    SizeChange Window.Size
  | Tick Float
  | Player Entity.Cat.Msg
  | Egg Entity.Egg.Msg
  | NoOp

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  let
    _ = 3 --Debug.log "update msg: " msg
  in
    case msg of
      Tick dt ->
        (step dt model, Cmd.batch <| effects dt model)
      SizeChange size ->
        { model | size = size } ! []
      Player catMsg ->
        map (System.Control.control Entity.Role.Cat (Entity.Cat.reduce catMsg)) model ! []
      Egg eggMsg ->
        map (Entity.Egg.reduce eggMsg) model ! []
      NoOp ->
        model ! []

step : Float -> Model -> Model
step dt model =
  --|> generateEggs input
  --|> withinViewport input
  ( map (Entity.boundFloor model.size)
    >> map (Entity.boundWalls model.size)
    >> map (System.Physics.gravity dt)
    >> map (System.Physics.newtonian dt)
    >> map System.Physics.clearForces
  ) model

-- TODO Should be List Msg, or just batch the messages?
-- TODO should filterMap after pairMap?
effects : Float -> Model -> List (Cmd Msg)
effects dt model =
  List.filterMap (System.Collision.detect interact) (pairs model.entities)

-- interactions

interact : (Entity.Role.Name, Entity.Model) -> (Entity.Role.Name, Entity.Model) -> Cmd Msg
interact (role1, entity1) (role2, entity2) =
  case role1 of
    Entity.Role.Cat ->
      Cmd.map Player <| Entity.Cat.interact (role1, entity1) (role2, entity2)
    Entity.Role.Egg ->
      Cmd.map Egg <| Entity.Egg.interact (role1, entity1) (role2, entity2)

----------------- View methods

view : Model -> Html msg
view model =
  let
    (w', h') = (model.size.width, model.size.height)
    (w, h) = (toFloat w', toFloat h')
    --_ = Debug.log "model in view" model
  in
    div []
    [
      toHtml
      <| Collage.collage w' h'
      <| List.filterMap System.View.render model.entities
    ]

----------------- Subscriptions

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch [
    Window.resizes SizeChange
  , AnimationFrame.diffs Tick
  , Keyboard.downs (keyboardRouter True)
  , Keyboard.ups (keyboardRouter False)
  ]

keyboardRouter : Bool -> Keyboard.KeyCode -> Msg
keyboardRouter isDown keyCode =
  case (Char.fromCode keyCode) of
    'W' ->
      Player <| Entity.Cat.Move Entity.Cat.Up
    'S' ->
      Player <| Entity.Cat.Move Entity.Cat.Down
    'A' ->
      Player <| Entity.Cat.Move Entity.Cat.Left
    'D' ->
      Player <| Entity.Cat.Move Entity.Cat.Right
    _ ->
      NoOp

--------------- Main functions

main : Program Never
main =
  App.program {
      init = init
    , update = update
    , view = view
    , subscriptions = subscriptions
    }

--port tasks : (Task Effects.Never ()) -> Cmd msg
--port tasks =
--  Signal.map (\effects ->
--    let
--      _ = Debug.log "port effects" <| Effects.batch effects
--    in
--      Effects.toTask actionInbox.address <| Effects.batch effects
--  ) effectSignal