import Window
import Mouse
import Input exposing (Input)
import Time exposing (Time)

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

createLabel : String -> Color -> Entity.Label
createLabel name colour = {
    name = name
  , color = colour
  }

initCursor : Entity
initCursor = {
    space = Component.createSpatial (0, 0) (0, 0) (0, 0)
  , corp = Component.createCorporeal (15, 15) Color.darkGray
  , control = \input space -> Component.setPos input.mouse space
  , view = \corp -> filled corp.color <| ngon 3 (fst corp.dim)
  , interactions = []
  , label = { name = "Cursor", color = Color.black }
  }

createTurtle : Vec.Vec -> Entity
createTurtle pos = {
    space = Component.createSpatial pos (0, -300) (0, 0)
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

createEgg : Vec -> Vec -> Entity
createEgg pos vel = {
    space = Component.createSpatial pos vel (0, 0)
  , corp = Component.createCorporeal (35, 35) Color.gray
  , control = \input space -> space
  , view = \corp ->
      group [
        filled corp.color <| circle ((fst corp.dim) / 2)
      ]
  , interactions = [
      (Entity.Egg, Entity.Egg)
    , (Entity.Egg, Entity.Cursor)
    ]
  , label = { name = "Egg", color = Color.black }
  }

bombLabeler : Entity
bombLabeler = {
    space = Component.createSpatial (0, 250) (0, 0) (0, 0)
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
    space = Component.createSpatial (-400 / 2 + 300 / 2, 50) (0, 0) (0, 0)
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
    space = Component.createSpatial (400 / 2 - 100 / 2, 50) (0, 0) (0, 0)
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
      createEgg (0, 300) (0, 0)
    , initCursor
    ]
  , seed = Random.initialSeed 0
  }


-------------- Update methods

update : Signal.Address Action -> (Input, Action) -> App -> App
update address (input, action) app =
  let
    _ = Debug.log "update action: " action
  in
    case action of
      Action.NoOp ->
        --sourceTurtles input app
        borderCollisionDetect input app
          |> collisionDetect address
          |> updateApp input
      Action.Entity entityAction ->
        { app |
          entities = updateEntities address (input, entityAction) app.entities
        }

updateEntities : Signal.Address Action -> (Input, EntityAction) -> List Entity -> List Entity
updateEntities address (input, action) entities =
  case action of
    Action.Open ->
      let
        _ = Debug.log "opened!" 3
      in
        List.map (\entity ->
            { entity |
              corp = Component.setColor Color.orange entity.corp
            }
          ) entities
    _ ->
      entities

sourceTurtles : Input -> App -> App
sourceTurtles input app =
  let
    (shouldCreate, newSeed1) = Random.generate Random.bool app.seed
    (xPos, newSeed0) = Random.generate (Random.float -300 300) newSeed1
    updatedEntities =
      if List.length app.entities < 60 && shouldCreate == True then
        createEgg (xPos, 300) (0, -300) :: app.entities
      else
        app.entities
  in
    { app | entities = updatedEntities , seed = newSeed0 }

borderCollisionDetect : Input -> App -> App
borderCollisionDetect input app =
  let
    -- NOTE refactor. very similary to inside()
    withinBounds entity =
      Vec.x entity.space.pos > -(toFloat <| fst input.window) / 2
      && Vec.x entity.space.pos < (toFloat <| fst input.window) / 2
      && Vec.y entity.space.pos > -(toFloat <| snd input.window) / 2
      && Vec.y entity.space.pos < (toFloat <| snd input.window) / 2
  in
    { app |
      entities = List.filter (withinBounds) app.entities
    }

collisionDetect : Signal.Address Action -> App -> App
collisionDetect address app =
  { app |
    entities = Collision.squaredUpdate (Collision.interact address) app.entities
  }

updateApp : Input -> App -> App
updateApp input app =
  { app |
    entities = List.map (simulatePhysics input << controlEntity input) app.entities
  }

controlEntity : Input -> Entity -> Entity
controlEntity input entity =
  { entity |
    space = entity.control input entity.space
  }

simulatePhysics : Input -> Entity -> Entity
simulatePhysics input entity =
  { entity |
    space =
      Component.setVel (entity.space.vel |+ entity.space.acc .* input.delta)
      >> Component.setPos (entity.space.pos |+ entity.space.vel .* input.delta)
      <| entity.space
  }

---------------- View methods

-- FIXME shouldn't have to pass in window dims. try using input instead
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

------------- Main functions

inbox : Signal.Mailbox Action.Action
inbox =
  Signal.mailbox Action.NoOp

actionInputSignal : Signal (Input, Action.Action)
actionInputSignal =
  Signal.map2 (\input action -> (input, action)) inputSignal inbox.signal

appState : Signal App
appState =
  Signal.foldp (update inbox.address) initApp actionInputSignal

main : Signal Element
main =
  Signal.map2 view Window.dimensions appState
