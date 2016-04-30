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
  , interactions = []
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
  , interactions = [
      (Entity.Turtle, Entity.Labeler)
    , (Entity.Turtle, Entity.Turtle)
    , (Entity.Turtle, Entity.Cursor)
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
  , interactions = [(Entity.Labeler, Entity.Turtle)]
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
  , interactions = [(Entity.Labeler, Entity.Turtle)]
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
  , interactions = [(Entity.Labeler, Entity.Turtle)]
  , label = createLabel "Not Ticking" Color.black
  }


type alias App =
  { entities: List Entity
  , seed: Random.Seed
  }

initApp : App
initApp = {
    entities = [
      Entity.Egg.create (0, 300) (0, 0)
    , initCursor
    ]
  , seed = Random.initialSeed 0
  }


-------------- Update methods

-- e -> acc -> acc
update : Signal.Address (List Action) -> (Input, List Action) -> (App, List (Effects Action)) -> (App, List (Effects Action))
update inboxAddress (input, actions) (app, _) =
  ({ app | entities = List.foldl actionateEntities app.entities actions } , [])
  -- sourceTurtles input app
  -- borderCollisionDetect input app
  |> collisionDetect inboxAddress
  |> updateApp input

-- Execute actions that were triggered by effects
actionateEntities : Action -> List Entity -> List Entity
actionateEntities action entities =
  case action of
    Action.NoOp ->
      entities
    Action.Entity entityAction ->
      List.map (actionateEntity entityAction) entities

actionateEntity : EntityAction -> Entity -> Entity
actionateEntity action entity =
  case action of
    Action.Open ->
      { entity | corp = Component.setColor Color.blue entity.corp }
    Action.Explode ->
      { entity | corp = Component.setColor Color.orange entity.corp }

sourceTurtles : Input -> App -> (App, Effects Action)
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

borderCollisionDetect : Input -> App -> (App, Effects Action)
borderCollisionDetect input app =
  let
    -- NOTE refactor. very similary to inside()
    withinBounds entity =
      Vec.x entity.space.pos > -(toFloat <| fst input.window) / 2
      && Vec.x entity.space.pos < (toFloat <| fst input.window) / 2
      && Vec.y entity.space.pos > -(toFloat <| snd input.window) / 2
      && Vec.y entity.space.pos < (toFloat <| snd input.window) / 2
  in
    ({ app |
        entities = List.filter (withinBounds) app.entities
      }
    , Effects.none)

collisionDetect : Signal.Address (List Action) -> (App, List (Effects Action)) -> (App, List (Effects Action))
collisionDetect inboxAddress (app, effects) =
  let
    newEffects = Collision.squaredUpdate (Collision.interact inboxAddress) app.entities
  in
    (app, newEffects)

updateApp : Input -> (App, List (Effects Action)) -> (App, List (Effects Action))
updateApp input (app, effects) =
  let
    newEntities = List.map (controlEntity input >> simulateEntity input) app.entities
  in
    ({ app | entities = newEntities } , effects)

controlEntity : Input -> Entity -> Entity
controlEntity input entity =
  { entity |
    space = entity.control input entity.space
  }

simulateEntity : Input -> Entity -> Entity
simulateEntity input entity =
  { entity |
    space =
      Component.setVel (entity.space.vel |+ entity.space.acc .* input.delta)
      >> Component.setPos (entity.space.pos |+ entity.space.vel .* input.delta)
      <| entity.space
  }

---------------- View methods

view : (Int, Int) -> App -> Element
view (width, height) app =
  collage width height
  <| List.map viewEntity app.entities

viewEntity : Entity -> Form
viewEntity entity =
  move entity.space.pos <| entity.view entity.corp

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

stateChangeSignal : Signal (App, List (Effects Action))
stateChangeSignal =
  Signal.foldp (update actionInbox.address) (initApp, []) actionInputSignal

appSignal : Signal App
appSignal =
  Signal.map fst stateChangeSignal

effectSignal : Signal (List (Effects Action))
effectSignal =
  Signal.map snd stateChangeSignal

------------- Main functions

main : Signal Element
main =
  Signal.map2 view Window.dimensions appSignal

port tasks : Signal (Task Effects.Never ())
port tasks =
  Signal.map (\effects ->
    let
      _ = Debug.log "effects" effects
      --Effects.toTask actionInbox.address <| Effects.batch effects
    in
      Effects.toTask actionInbox.address <| Maybe.withDefault Effects.none <| List.head effects
  ) effectSignal
