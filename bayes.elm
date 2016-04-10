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

import Spatial
import Corporeal
import Vec

import Debug


-------------- Model methods

type Role = Cursor | Turtle | Labeler

type alias Interaction = (Role, Role)

type alias Control = Input -> Spatial.Spatial -> Spatial.Spatial

type alias CorporealView = Corporeal.Corporeal -> Form

type alias Entity =
  { space: Spatial.Spatial
  , corp: Corporeal.Corporeal
  , control: Control
  , view: CorporealView
  , interactions: List Interaction
  , label: Label
  }

type alias Label = {
    name : String
  , color : Color
  }

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
  , label = { name = "", color = Color.black }
  }

createTurtle : Vec.Vec -> Entity
createTurtle pos = {
    space = Spatial.createSpatial pos
  , corp = Corporeal.initCorporeal
  , control = \input space ->
      Spatial.setPos (Vec.x space.pos, (Vec.y space.pos) - 300 * input.delta) space
  , view = \corp ->
      filled corp.color <| circle 10
  , interactions = []
  , label = { name = "", color = Color.black }
  }


initLabeler : Entity
initLabeler = {
    space = Spatial.createSpatial (0, 200)
  , corp = Corporeal.createCorporeal (300, 20) Color.red
  , control = \input space -> space
  , view = \corp ->
      filled corp.color <| (uncurry rect) corp.dim
  , interactions = []
  , label = createLabel "Cancer" Color.black
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

inside : Entity -> Entity -> Bool
inside expectedEntity testedEntity =
  let
    testedPos = testedEntity.space.pos
    expectedPos = expectedEntity.space.pos
    testedDim = testedEntity.corp.dim
    expectedDim = expectedEntity.corp.dim
  in
    if (Vec.y testedPos > (Vec.y expectedPos - Vec.y expectedDim / 2)
      && Vec.y testedPos < (Vec.y expectedPos + Vec.y expectedDim / 2)
      && Vec.x testedPos > (Vec.x expectedPos - Vec.x expectedDim / 2)
      && Vec.x testedPos < (Vec.x expectedPos + Vec.x expectedDim / 2)) then
      True
    else
      False

collide : Entity -> Entity -> Entity
collide other self =
  if inside self other then
    -- run through all interactions of self and update self
    -- run through all interactions of other and update other
    self
  else
    self

-- interactionCallback : other -> target -> target
pairwiseUpdate : (Entity -> Entity -> Entity) -> List Entity -> List Entity
pairwiseUpdate interactionCallback entities =
  if List.length entities <= 1 then
    entities
  else
    List.indexedMap (\index entity ->
      if index == 0 then
        entity
      else
        List.foldl interactionCallback entity
        <| List.drop (index + 1) entities
    ) entities


collisionDetect : App -> App
collisionDetect app =
  { app | entities = pairwiseUpdate collide app.entities }


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

type alias Input =
  { window: (Int, Int)
  , mouse : (Float, Float)
  , delta: Time
  }

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
