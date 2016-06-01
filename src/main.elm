import Window
import AnimationFrame
import Mouse
import Random
import Keyboard
import Task
import Char

import Time exposing (Time)
import Basics.Extra exposing (never)
import List.Extra exposing (pairs)

import Collage
import Element exposing (toHtml)
import Transform exposing (Transform, multiply)

import Html exposing (..)
import Html.App as App

import Entity
import Entity.Camera
import Entity.Role
import Entity.Egg
import Entity.Cat

import System.Physics
import System.Control
import System.Renderer
import System.Collision
--import Viewport

import Debug

-------------- Model methods

type alias Model = {
    entities : List Entity.Model
  , camera : Entity.Model
  , nextEntityId : Int
  , seed : Random.Seed
  , size : Window.Size
  , accFrameTime : Float -- not used
  , targetFrameRate : Float -- in ms
  }

init : (Model, Cmd Msg)
init =
  ({
      entities = [
        Entity.Cat.init 0
      , Entity.Egg.init 11 (-300, 100) (10, 0)
      , Entity.Egg.init 12 (-200, 100) (5, 10)
      , Entity.Egg.init 13 (-100, 100) (-10, -10)
      , Entity.Egg.init 14 (100, 100) (10, -5)
      , Entity.Egg.init 15 (200, 100) (2, 5)
      , Entity.Egg.init 16 (300, 100) (-4, 2)
      ]
    , camera = Entity.Camera.init 1
    , nextEntityId = 7
    , seed = Random.initialSeed 0
    , size = Window.Size 0 0
    , accFrameTime = 0.0
    , targetFrameRate = 1000 / 60.0
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
  | Simulate Float
  | Player Entity.Cat.Msg
  | Egg Entity.Egg.Msg
  | Camera Entity.Camera.Msg
  | NoOp

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  let
    _ = 3 --Debug.log "update msg: " msg
  in
    case msg of
      Tick dt ->
        stablizeFrameRate dt model
      Simulate dt ->
        (step dt model, Cmd.batch <| effects dt model)
      SizeChange size ->
        { model | size = size } ! []
      Player catMsg ->
        map (System.Control.control Entity.Role.Cat (Entity.Cat.reduce catMsg)) model ! []
      Egg eggMsg ->
        map (Entity.Egg.reduce eggMsg) model ! []
      Camera cameraMsg ->
        model ! []
      NoOp ->
        model ! []

{-| Stablizes the framerate.
TODO only did semi-fixed timestep. Seems good enough for now.
http://gafferongames.com/game-physics/fix-your-timestep/
-}
stablizeFrameRate : Float -> Model -> (Model, Cmd Msg)
stablizeFrameRate frameTime model =
  let
    dt = min frameTime model.targetFrameRate
  in
    if frameTime <= 0.0 then
      model ! []
    else
      model ! [
        Task.perform never identity (Task.succeed (Tick <| frameTime - dt))
      , Task.perform never identity (Task.succeed (Simulate dt))
      ]

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
    _ ->
      Task.perform never identity (Task.succeed NoOp)

----------------- View methods

view : Model -> Html msg
view model =
  let
    (w', h') = (model.size.width, model.size.height)
    (w, h) = (toFloat w', toFloat h')
    --_ = Debug.log "model in view" model
  in
    toHtml
    <| Collage.collage w' h'
    <| List.filterMap (System.Renderer.render model.camera) model.entities

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
