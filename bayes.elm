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
  , corp = Corporeal.createCorporeal (20, 20) Color.gray
  , control = \input space ->
      Spatial.setPos (Vec.x space.pos, (Vec.y space.pos) - 300 * input.delta) space
  , view = \corp ->
      filled corp.color <| circle ((fst corp.dim) / 2)
  , interactions = [(Turtle, Labeler)]
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

-- FIXME bug. it doesn't calculate intersections of bounding boxes
inside : Entity -> Entity -> Bool
inside self other =
  let
    selfPos = self.space.pos
    otherPos = other.space.pos
    selfDim = self.corp.dim
    otherDim = other.corp.dim
  in
    if  (Vec.y otherPos > (Vec.y selfPos - Vec.y selfDim / 2)
      && Vec.y otherPos < (Vec.y selfPos + Vec.y selfDim / 2)
      && Vec.x otherPos > (Vec.x selfPos - Vec.x selfDim / 2)
      && Vec.x otherPos < (Vec.x selfPos + Vec.x selfDim / 2)) then
      True
    else
      False

routeInteraction : Interaction -> (Entity -> Entity -> Entity)
routeInteraction interaction =
  case interaction of
    (Turtle, Labeler) -> iaTurtleLabeler
    _ -> iaNoOp

iaTurtleLabeler : Entity -> Entity -> Entity
iaTurtleLabeler self other =
  { self | corp = Corporeal.setColor Color.green self.corp }

iaNoOp : Entity -> Entity -> Entity
iaNoOp self other = self

collide : Entity -> Entity -> Entity
collide other self =
  if inside other self then
    -- run through all interactions of self and update self
    List.foldl (\interaction entity ->
      routeInteraction interaction entity other
    ) self self.interactions

    -- run through all interactions of other and update other
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

squaredUpdate : (Entity -> Entity -> Entity) -> List Entity -> List Entity
squaredUpdate interactionCallback entities =
  List.indexedMap (\index entity ->
    List.foldl (\other self ->
      interactionCallback other self
    ) entity <| List.append (List.take index entities) (List.drop (index + 1) entities)
  ) entities

collisionDetect : App -> App
collisionDetect app =
  { app | entities = squaredUpdate collide app.entities }


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
