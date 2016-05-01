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

import Entity exposing (Entity)
import Component
import Collision
import Vec exposing (..)
import Action exposing (Action, EntityAction)


import Debug

-------------- Model methods

import Entity.Egg

createLabel : String -> Color -> Entity.Label
createLabel name colour = {
    name = name
  , color = colour
  }

initCursor : Entity
initCursor = {
    role = Entity.Cursor
  , space = Component.createSpatial (0, 0) (0, 0) (0, 0)
  , corp = Component.createCorporeal (15, 15) Color.darkGray
  , control = \input space -> Component.setPos input.mouse space
  , view = \corp -> filled corp.color <| ngon 3 (fst corp.dim)
  , label = { name = "Cursor", color = Color.black }
  }

createTurtle : Vec.Vec -> Entity
createTurtle pos = {
    role = Entity.Turtle
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
    role = Entity.Labeler
  , space = Component.createSpatial (0, 250) (0, 0) (0, 0)
  , corp = Component.createCorporeal (400, 20) Color.lightRed
  , control = \input space -> space
  , view = \corp ->
      group [
        filled corp.color <| (uncurry rect) corp.dim
      ]
  , label = createLabel "Bomb" Color.black
  }

tickingLabeler : Entity
tickingLabeler = {
    role = Entity.Labeler
  , space = Component.createSpatial (-400 / 2 + 300 / 2, 50) (0, 0) (0, 0)
  , corp = Component.createCorporeal (300, 20) Color.lightPurple
  , control = \input space -> space
  , view = \corp ->
      group [
        filled corp.color <| (uncurry rect) corp.dim
      ]
  , label = createLabel "Ticking" Color.black
  }

notTickingLabeler : Entity
notTickingLabeler = {
    role = Entity.Labeler
  , space = Component.createSpatial (400 / 2 - 100 / 2, 50) (0, 0) (0, 0)
  , corp = Component.createCorporeal (100, 20) Color.lightGreen
  , control = \input space -> space
  , view = \corp ->
      group [
        filled corp.color <| (uncurry rect) corp.dim
      ]
  , label = createLabel "Not Ticking" Color.black
  }


type alias AppState =
  { entities: List Entity
  , seed: Random.Seed
  }

type alias App = (AppState, List (Effects Action))

initApp : AppState
initApp = {
    entities = [
      Entity.Egg.create (0, 300) (0, 0)
    , initCursor
    ]
  , seed = Random.initialSeed 0
  }


-------------- Update methods

-- e -> acc -> acc
update : Signal.Address (List Action) -> (Input, List Action) -> App -> App
update inboxAddress (input, actions) (app, _) =
  ({ app | entities = List.foldl actionateEntities app.entities actions } , [])
  -- sourceTurtles input
  |> borderCollisionDetect input
  |> collisionDetect inboxAddress
  |> updateApp input

-- Execute actions that were triggered by effects
actionateEntities : Action -> List Entity -> List Entity
actionateEntities action entities =
  case action of
    Action.NoOp ->
      entities
    Action.Entity entityAction ->
      List.map (Entity.actionate entityAction) entities
    Action.Egg eggAction ->
      List.map (Entity.Egg.actionate eggAction) entities

sourceTurtles : Input -> AppState -> (AppState, Effects Action)
sourceTurtles input app =
  let
    (shouldCreate, newSeed1) = Random.generate Random.bool app.seed
    (xPos, newSeed0) = Random.generate (Random.float -300 300) newSeed1
    updatedEntities =
      if List.length app.entities < 60 && shouldCreate == True then
        Entity.Egg.create (xPos, 300) (0, -300) :: app.entities
      else
        app.entities
  in
    ({ app | entities = updatedEntities , seed = newSeed0 }
    , Effects.none)

borderCollisionDetect : Input -> App -> App
borderCollisionDetect input (appState, effects) =
  let
    top = (toFloat <| snd input.window) / 2
    bottom = -(toFloat <| snd input.window) / 2
    left = -(toFloat <| fst input.window) / 2
    right = (toFloat <| fst input.window) / 2
  in
    ({ appState |
      entities = List.filter (\entity ->
        borderCollisionExemptions entity
        || Collision.withinBounds (top, right, bottom, left) entity
      ) appState.entities
     }
    , effects)

borderCollisionExemptions : Entity -> Bool
borderCollisionExemptions entity =
  case entity.role of
    Entity.Cursor ->
      True
    _ ->
      False

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
  Signal.map2 (\input actions -> (input, actions)) inputSignal actionInbox.signal

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
      _ = Debug.log "effects" effects
      --Effects.toTask actionInbox.address <| Effects.batch effects
    in
      Effects.toTask actionInbox.address <| Maybe.withDefault Effects.none <| List.head effects
  ) effectSignal
