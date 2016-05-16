import Window
import AnimationFrame
import Mouse
import Random
import Task

import Time exposing (Time)

import Collage
import Element exposing (..)
import Color exposing (Color)
import Text

import Html exposing (..)
import Html.App as App


--import Input exposing (Input)
--import Collision
--import Viewport
--import Vec exposing (..)
--import Role
--import Entity exposing (Entity)
--import Component

import Entity
import Entity.Egg

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
        Entity.Egg.init 1
       ]
    , nextEntityId = 0
    , seed = Random.initialSeed 0
    , size = Window.Size 0 0
    }
  , Task.perform (\_ -> NoOp) SizeChange Window.size
  )

-------------- Update methods

type Msg =
    SizeChange Window.Size
  | Tick Float
  | NoOp

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    SizeChange size ->
      ({ model | size = size }, Cmd.none)
    Tick dt ->
      (step dt model, Cmd.none)
    NoOp ->
      (model, Cmd.none)

step : Float -> Model -> Model
step dt model =
  --reduceAppState appState actions
  --|> generateEggs input
  --|> withinViewport input
  --|> collisionDetect inboxAddress
  model
  |> gravity dt
  |> newtonian dt

gravity : Float -> Model -> Model
gravity dt model =
  { model | entities = List.map (Entity.gravity dt) model.entities }

newtonian : Float -> Model -> Model
newtonian dt model =
  { model | entities = List.map (Entity.newtonian dt) model.entities }

--updateApp : Input -> App -> App
--updateApp input (appState, effects) =
--  ({ appState |
--    entities =
--      Entity.EntityList.map (Entity.control input >> Entity.simulate input) appState.entities
--  } , effects)

---- Execute actions that were triggered by effects
--
--reduceAppState : AppState -> (List Action) -> App
--reduceAppState appState actions =
--  ({ appState |
--    entities = List.foldl reduce appState.entities actions
--  }, [])
--
--reduce : Action -> Entity.EntityList.Model -> Entity.EntityList.Model
--reduce action entities =
--  case action of
--    Action.NoOp ->
--      entities
--    Action.Egg eggAction ->
--      entities
--    Action.EntityList entityListAction ->
--      Entity.EntityList.reduce entityListAction entities
--
--debugEffects : App -> App
--debugEffects (appState, effects) =
--  let
--    _ = Debug.log "effects: " <| effects
--  in
--    (appState, effects)
--
--generateEggs : Input -> App -> App
--generateEggs input (appState, effects) =
--  let
--    newEffect = Effects.task
--      <| Task.succeed (Action.EntityList Entity.EntityList.Insert)
--  in
--    (appState, newEffect :: effects)
--
----withinViewport : Input -> App -> App
----withinViewport input (appState, effects) =
----  ({ appState |
----     entities = Viewport.cull input appState.entities
----   }
----  , effects)
--
--collisionDetect : Signal.Address (List Action) -> App -> App
--collisionDetect inboxAddress (appState, effects) =
--  let
--    newEffects = Collision.pairMap (Collision.interact inboxAddress) appState.entities
--  in
--    (appState, List.append newEffects effects)
--
--

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


---------------- Input methods
--
--delta : Signal Time
--delta = Signal.map Time.inSeconds (Time.fps 30)
--
--screenToWorld : (Int, Int) -> (Int, Int) -> (Float, Float)
--screenToWorld (width, height) (x, y) =
--  ((toFloat x) - (toFloat width) / 2,
--  -(toFloat y) + (toFloat height) / 2)
--
--inputSignal : Signal Input
--inputSignal =
--  Signal.sampleOn delta <|
--    Signal.map3 Input
--      (Window.dimensions)
--      (Signal.map2 screenToWorld Window.dimensions Mouse.position)
--      delta
--
--actionInbox : Signal.Mailbox (List Action)
--actionInbox =
--  Signal.mailbox []
--
--actionInputSignal : Signal (Input, List Action)
--actionInputSignal =
--  Signal.map2 (\input actions -> (input, actions))
--    inputSignal
--    (Signal.sampleOn delta actionInbox.signal)
--
--appSignal : Signal App
--appSignal =
--  Signal.foldp (update actionInbox.address) (initApp, []) actionInputSignal
--
--stateSignal : Signal AppState
--stateSignal =
--  Signal.map fst appSignal
--
--effectSignal : Signal (List (Effects Action))
--effectSignal =
--  Signal.map snd appSignal
--

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch [
    Window.resizes SizeChange
  , AnimationFrame.diffs Tick
  ]

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
