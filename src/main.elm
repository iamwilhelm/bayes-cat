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
import Entity.Role
import Entity.Camera
import Entity.Platform
import Entity.Cat
import Entity.Egg

import System.Physics
import System.Control
import System.Renderer
import System.Collision
--import Viewport

import Debug

-------------- Model methods

type alias Model = {
    entities : List Entity.Model
  , nextEntityId : Int
  , seed : Random.Seed
  , size : Window.Size
  , accFrameTime : Float -- frame time accumulator
  , targetFrameRate : Float -- in ms (constant)
  }

init : (Model, Cmd Msg)
init =
  ({
      entities = [
        Entity.Camera.init 1
      , Entity.Platform.init 10 (-100, -150)
      , Entity.Cat.init 20
      , Entity.Egg.init 21 (-200, 100) (0, -1)
      , Entity.Egg.init 22 (-200, -150) (0, 4)
      --, Entity.Egg.init 23 (-100, 100) (-10, -10)
      --, Entity.Egg.init 24 (100, 100) (10, -5)
      --, Entity.Egg.init 25 (200, 100) (2, 5)
      --, Entity.Egg.init 26 (300, 100) (-4, 2)
      ]
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
  | NewFrame Float
  | Tick
  | Simulate Float
  | Player Entity.Cat.Msg
  | Egg Entity.Egg.Msg
  | Camera Entity.Camera.Msg
  | NoOp

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    NewFrame dt ->
      { model | accFrameTime = model.accFrameTime + dt }
      ! [Task.perform never identity (Task.succeed Tick)]
    Tick ->
      stablizeFrameRate model
    Simulate dt ->
      (step dt model, Cmd.batch <| effects dt model)
    SizeChange size ->
      { model | size = size } ! []
    Player catMsg ->
      map (System.Control.control Entity.Role.Cat (Entity.Cat.reduce catMsg)) model ! []
    Egg eggMsg ->
      map (Entity.Egg.reduce eggMsg) model ! []
    Camera cameraMsg ->
      map (System.Control.control Entity.Role.Camera (Entity.Camera.reduce cameraMsg)) model ! []
    NoOp ->
      model ! []

{-| Stablizes the framerate.
http://gafferongames.com/game-physics/fix-your-timestep/
-}
stablizeFrameRate : Model -> (Model, Cmd Msg)
stablizeFrameRate model =
  let
    dt = min model.accFrameTime model.targetFrameRate
  in
    if model.accFrameTime < model.targetFrameRate then
      model ! []
    else
      { model |
        accFrameTime = model.accFrameTime - dt
      } ! [
        Task.perform never identity (Task.succeed (Simulate dt))
      , Task.perform never identity (Task.succeed Tick)
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

interact : (Entity.Role.Name, Bool, Entity.Model) -> (Entity.Role.Name, Bool, Entity.Model) -> Cmd Msg
interact (role1, isColliding1, entity1) (role2, isColliding2, entity2) =
  case role1 of
    Entity.Role.Cat ->
      Cmd.map Player
      <| Entity.Cat.interact (role1, isColliding1, entity1) (role2, isColliding2, entity2)
    Entity.Role.Egg ->
      Cmd.map Egg
      <| Entity.Egg.interact (role1, isColliding1, entity1) (role2, isColliding2, entity2)
    _ ->
      Task.perform never identity (Task.succeed NoOp)

----------------- View methods

view : Model -> Html msg
view model =
  let
    (w', h') = (model.size.width, model.size.height)
    (w, h) = (toFloat w', toFloat h')
    camera = getCamera model
    --_ = Debug.log "model in view" model
  in
    toHtml
    <| Collage.collage w' h'
    <| List.filterMap (System.Renderer.render camera) model.entities

-- temp
getCamera : Model -> Maybe Entity.Model
getCamera model =
  List.head (
    List.filterMap (\entity ->
      case (Entity.getControllable entity) of
        Just ctrl ->
          if ctrl.role == Entity.Role.Camera then
            Just entity
          else
            Nothing
        Nothing ->
          Nothing
    ) model.entities
  )

----------------- Subscriptions

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch [
    Window.resizes SizeChange
  , AnimationFrame.diffs NewFrame
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
    'I' ->
      Camera <| Entity.Camera.Move Entity.Camera.Up
    'K' ->
      Camera <| Entity.Camera.Move Entity.Camera.Down
    'J' ->
      Camera <| Entity.Camera.Move Entity.Camera.Left
    'L' ->
      Camera <| Entity.Camera.Move Entity.Camera.Right
    'U' ->
      Camera <| Entity.Camera.Zoom Entity.Camera.In
    'O' ->
      Camera <| Entity.Camera.Zoom Entity.Camera.Out
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
