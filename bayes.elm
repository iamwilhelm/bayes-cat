import Html exposing (..)
import Html.Attributes exposing (..)
import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Window
import Mouse
import Time exposing (..)
import List
import Random

import Entity exposing (..)
import Vec

import Spatial
import Corporeal
import Vec


import Collision
import Input exposing (..)

import Debug

-------------- Model methods

createLabel : String -> Color -> Label
createLabel name colour = {
    name = name
  , color = colour
  }

initCursor : Entity
initCursor = {
    space = Spatial.initSpatial
  , corp = Corporeal.initCorporeal
  , control = \input space -> Spatial.setPos input.mouse space
  , view = \corp -> filled corp.color <| ngon 3 10
  , interactions = []
  , label = { name = "Cursor", color = Color.black }
  }

createTurtle : Vec.Vec -> Entity
createTurtle pos = {
    space = Spatial.createSpatial pos
  , corp = Corporeal.createCorporeal (20, 20) Color.gray
  , control = \input space ->
      let
        newX = Vec.x space.pos + Vec.x space.vel * input.delta
        newY = Vec.y space.pos + Vec.y space.vel * input.delta
      in
        Spatial.setPos (newX, newY) space
  , view = \corp ->
      filled corp.color <| circle ((fst corp.dim) / 2)
  , interactions = [(Turtle, Labeler), (Turtle, Turtle), (Turtle, Cursor)]
  , label = { name = "Turtle", color = Color.black }
  }

initLabeler : Entity
initLabeler = {
    space = Spatial.createSpatial (0, 200)
  , corp = Corporeal.createCorporeal (300, 20) Color.red
  , control = \input space -> space
  , view = \corp ->
      filled corp.color <| (uncurry rect) corp.dim
  , interactions = []
  , label = createLabel "Labeler" Color.black
  }

type alias App =
  { entities: List Entity
  , seed: Random.Seed
  }

initApp : App
initApp = {
    entities = [initCursor, initLabeler]
  , seed = Random.initialSeed 0
  }


-------------- Update methods

update : Input -> App -> App
update input app =
  sourceTurtles input app
    |> borderCollisionDetect input
    |> collisionDetect
    |> updateEntities input

sourceTurtles : Input -> App -> App
sourceTurtles input app =
  let
    (shouldCreate, newSeed1) = Random.generate Random.bool app.seed
    (xPos, newSeed0) = Random.generate (Random.float -300 300) newSeed1
    updatedEntities =
      if List.length app.entities < 60 && shouldCreate == True then
        createTurtle (xPos, 300) :: app.entities
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
      entities = List.filter withinBounds app.entities
    }

collisionDetect : App -> App
collisionDetect app =
  { app | entities = Collision.squaredUpdate Collision.collide app.entities }


updateEntities : Input -> App -> App
updateEntities input app =
  { app |
    entities = List.map (updateEntity input) app.entities
  }

updateEntity : Input -> Entity -> Entity
updateEntity input entity =
  { entity |
    space = entity.control input entity.space
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
delta = Signal.map inSeconds (fps 30)

screenToWorld : (Int, Int) -> (Int, Int) -> (Float, Float)
screenToWorld (width, height) (x, y) =
  ((toFloat x) - (toFloat width) / 2,
  -(toFloat y) + (toFloat height) / 2)

input : Signal Input
input =
  Signal.sampleOn delta <|
    Signal.map3 Input
      (Window.dimensions)
      (Signal.map2 screenToWorld Window.dimensions Mouse.position)
      delta

------------- Main functions

appState : Signal App
appState =
  Signal.foldp update initApp input

main : Signal Element
main =
  Signal.map2 view Window.dimensions appState
