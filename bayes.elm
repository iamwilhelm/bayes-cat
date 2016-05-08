import Window
import Mouse
import Input exposing (Input)
import Time exposing (Time)
import Task exposing (Task)
import Effects exposing (Effects)

import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Color exposing (Color)
import Text

import Html exposing (..)
import Html.Attributes exposing (..)

import List
import Random

import Collision
import Viewport
import Vec exposing (..)
import Action exposing (Action)
import Role
import Entity exposing (Entity)
import Component

import Entity.Egg
import Entity.Pointer

import Debug

-------------- Model methods

createTurtle : Vec.Vec -> Entity
createTurtle pos = {
    role = Role.Turtle
  , space = Component.createSpatial pos (0, -300) (0, 0)
  , corp = Component.createCorporeal (30, 30) Color.green
  , control = \input space ->
      space
  , view = \corp ->
      group [
        filled corp.color <| circle ((fst corp.dim) / 2)
      ]
  , label = { name = "Turtle", color = Color.black }
  }

bombLabeler : Entity
bombLabeler = {
    role = Role.Labeler
  , space = Component.createSpatial (0, 250) (0, 0) (0, 0)
  , corp = Component.createCorporeal (400, 20) Color.lightRed
  , control = \input space -> space
  , view = \corp ->
      group [
        filled corp.color <| (uncurry rect) corp.dim
      ]
  , label = Component.createLabel "Bomb" Color.black
  }

type alias AppState =
  { entities: List Entity
  , seed: Random.Seed
  }

type alias App = (AppState, List (Effects Action))

initApp : AppState
initApp = {
    entities = [
      Entity.Pointer.init
    ]
  , seed = Random.initialSeed 0
  }


-------------- Update methods

-- e -> acc -> acc
update : Signal.Address (List Action) -> (Input, List Action) -> App -> App
update inboxAddress (input, actions) (appState, _) =
  let
    _ = 3 --Debug.log "entities: " <| List.length appState.entities
    asdf = 4 --Debug.log "actions: " actions
  in
    ({ appState | entities = List.foldl reduce appState.entities actions } , [])
    |> generateEggs input
    |> withinViewport input
    |> collisionDetect inboxAddress
    |> updateApp input

-- Execute actions that were triggered by effects

reduce : Action -> List Entity -> List Entity
reduce action entities =
  case action of
    Action.NoOp ->
      entities
    Action.Egg eggAction ->
      List.map (\entity ->
        if entity.role == Role.Egg then
          Entity.Egg.reduce eggAction entity
        else
          entity
      ) entities

debugEffects : App -> App
debugEffects (appState, effects) =
  let
    _ = Debug.log "effects: " <| effects
  in
    (appState, effects)

generateEggs : Input -> App -> App
generateEggs input (appState, effects) =
  let
    (shouldCreate, newSeed1) = Random.generate Random.bool appState.seed
    (xPos, newSeed0) = Random.generate (Random.float -300 300) newSeed1
    updatedEntities =
      if List.length appState.entities < 5 && shouldCreate == True then
        Entity.Egg.create (xPos, 300) (0, -300) :: appState.entities
      else
        appState.entities
  in
    ({ appState | entities = updatedEntities , seed = newSeed0 }
    , effects)

withinViewport : Input -> App -> App
withinViewport input (appState, effects) =
    ({ appState | entities = Viewport.cull input appState.entities }
    , effects)

collisionDetect : Signal.Address (List Action) -> App -> App
collisionDetect inboxAddress (appState, effects) =
  let
    newEffects = Collision.squaredUpdate (Collision.interact inboxAddress) appState.entities
  in
    (appState, newEffects)

updateApp : Input -> App -> App
updateApp input (appState, effects) =
  let
    newEntities = List.map (Entity.control input >> Entity.simulate input) appState.entities
  in
    ({ appState | entities = newEntities } , effects)

---------------- View methods

view : (Int, Int) -> AppState -> Element
view (width, height) app =
  collage width height
  <| List.map Entity.view app.entities

-------------- Input methods

delta : Signal Time
delta = Signal.map Time.inSeconds (Time.fps 30)

screenToWorld : (Int, Int) -> (Int, Int) -> (Float, Float)
screenToWorld (width, height) (x, y) =
  ((toFloat x) - (toFloat width) / 2,
  -(toFloat y) + (toFloat height) / 2)

inputSignal : Signal Input
inputSignal =
  Signal.sampleOn delta <|
    Signal.map3 Input
      (Window.dimensions)
      (Signal.map2 screenToWorld Window.dimensions Mouse.position)
      delta

actionInbox : Signal.Mailbox (List Action)
actionInbox =
  Signal.mailbox []

actionInputSignal : Signal (Input, List Action)
actionInputSignal =
  Signal.map2 (\input actions -> (input, actions))
    inputSignal
    (Signal.sampleOn delta actionInbox.signal)

appSignal : Signal App
appSignal =
  Signal.foldp (update actionInbox.address) (initApp, []) actionInputSignal

stateSignal : Signal AppState
stateSignal =
  Signal.map fst appSignal

effectSignal : Signal (List (Effects Action))
effectSignal =
  Signal.map snd appSignal

------------- Main functions

main : Signal Element
main =
  Signal.map2 view Window.dimensions stateSignal

port tasks : Signal (Task Effects.Never ())
port tasks =
  Signal.map (\effects ->
    let
      _ = Debug.log "port effects" <| Effects.batch effects
    in
      Effects.toTask actionInbox.address <| Effects.batch effects
  ) effectSignal
