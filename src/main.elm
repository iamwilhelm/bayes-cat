import Window
import AnimationFrame
import Mouse
import Random
import Keyboard
import Task
import Char

import Time exposing (Time)

import Collage
import Element exposing (..)
import Color exposing (Color)
import Text

import Html exposing (..)
import Html.App as App

import Entity
import Entity.Role
import Entity.Egg
import Entity.Cat

import System.Physics
import System.Control
--import System.Collision
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
        Entity.Cat.init 1
      , Entity.Egg.init 2
      ]
    , nextEntityId = 3
    , seed = Random.initialSeed 0
    , size = Window.Size 0 0
    }
  , Task.perform (\_ -> NoOp) SizeChange Window.size
  )

map : (Entity.Model -> Entity.Model) -> Model -> Model
map func model =
  { model | entities = List.map func model.entities }

--pairMap : (Entity.Model -> Entity.Model -> a) -> Model -> List a
--pairMap func model =
--  { model |
--    entities = System.Collision.pairMap
--  }
-------------- Update methods

type Msg =
    SizeChange Window.Size
  | Tick Float
  | Player Entity.Cat.Msg
  | Egg Entity.Egg.Msg
  | NoOp

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  let
    b = 3--Debug.log "update msg: " msg
  in
    case msg of
      SizeChange size ->
        { model | size = size } ! []
      Tick dt ->
        step dt model ! []
      Player catMsg ->
        { model |
          entities =
            System.Control.reduceMap Entity.Role.Cat (Entity.Cat.reduce catMsg) model.entities
        } ! []
      Egg eggMsg ->
        { model |
          entities =
            System.Control.reduceMap Entity.Role.Egg (Entity.Egg.reduce eggMsg) model.entities
        } ! []
      NoOp ->
        (model, Cmd.none)

step : Float -> Model -> Model
step dt model =
  --reduceAppState appState actions
  --|> generateEggs input
  --|> withinViewport input
  --|> collisionDetect inboxAddress
  ( map (Entity.boundFloor model.size)
    >> map (System.Physics.gravity dt)
    >> map (System.Physics.newtonian dt)
    >> map System.Physics.clearForces
  ) model

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
      <| List.filterMap Entity.view model.entities
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
