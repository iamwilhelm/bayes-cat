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

type Entity = Cursor EntityData
  | Turtle EntityData
  | Labeler EntityData

type alias EntityData =
  { space: Spatial.Spatial
  , corp: Corporeal.Corporeal
  , label: Label
  }

initCursor =
  Cursor {
    space = Spatial.initSpatial
  , corp = Corporeal.initCorporeal
  , label = { name = "", color = Color.black }
  }

initTurtle =
  Turtle {
    space = Spatial.createSpatial (0, 400)
  , corp = Corporeal.initCorporeal
  , label = { name = "", color = Color.black }
  }

createTurtle : Vec.Vec -> Entity
createTurtle pos =
  Turtle {
    space = Spatial.createSpatial pos
  , corp = Corporeal.initCorporeal
  , label = { name = "", color = Color.black }
  }

type alias Label =
  { name : String
  , color : Color
  }

createLabel : String -> Color -> Label
createLabel name colour =
  { name = name, color = colour }


initLabeler : Entity
initLabeler =
  Labeler {
    space = Spatial.createSpatial (0, 200)
  , corp = Corporeal.createCorporeal (300, 20) Color.blue
  , label = createLabel "Cancer" Color.red
  }

type alias App =
  { entities: List Entity
  , seed: Random.Seed
  }

initApp : App
initApp = {
    entities = [ initCursor , initLabeler ]
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
    withinBounds entity =
      case entity of
        Cursor _ ->
          True
        Labeler _ ->
          True
        Turtle data ->
          Vec.x data.space.pos > -(toFloat <| fst input.window) / 2
          && Vec.x data.space.pos < (toFloat <| fst input.window) / 2
          && Vec.y data.space.pos > -(toFloat <| snd input.window) / 2
          && Vec.y data.space.pos < (toFloat <| snd input.window) / 2
  in
    { app |
      entities = List.filter withinBounds app.entities
    }

labelerEntities : List Entity -> List Entity
labelerEntities entities =
  let
    isLabeler entity =
      case entity of
        Labeler data ->
          True
        _ ->
          False
  in
    List.filter isLabeler entities

collisionDetect : App -> App
collisionDetect app =
  { app |
    entities = List.map (collideEntity <| labelerEntities app.entities) app.entities
  }

extractSpatial : Entity -> Spatial.Spatial
extractSpatial entity =
  case entity of
    Cursor data ->
      data.space
    Turtle data ->
      data.space
    Labeler data ->
      data.space

extractCorporeal : Entity -> Corporeal.Corporeal
extractCorporeal entity =
  case entity of
    Cursor data ->
      data.corp
    Turtle data ->
      data.corp
    Labeler data ->
      data.corp

inside : Entity -> Entity -> Bool
inside expectedEntity testedEntity =
  let
    testedPos = (extractSpatial testedEntity).pos
    expectedPos = (extractSpatial expectedEntity).pos
    testedDim = (extractCorporeal testedEntity).dim
    expectedDim = (extractCorporeal expectedEntity).dim
  in
    if (Vec.y testedPos > (Vec.y expectedPos - Vec.y expectedDim / 2)
      && Vec.y testedPos < (Vec.y expectedPos + Vec.y expectedDim / 2)
      && Vec.x testedPos > (Vec.x expectedPos - Vec.x expectedDim / 2)
      && Vec.x testedPos < (Vec.x expectedPos + Vec.x expectedDim / 2)) then
      True
    else
      False

collideWithEffect : (EntityData -> EntityData) -> Entity -> Entity -> Entity
collideWithEffect effectCallback expectedEntity testedEntity =
  if (inside expectedEntity testedEntity) then
    case testedEntity of
      Cursor data -> Cursor <| effectCallback data
      Turtle data -> Turtle <| effectCallback data
      Labeler data -> Labeler <| effectCallback data
  else
    testedEntity


collideEntity : List Entity -> Entity -> Entity
collideEntity entities entityAcc =
  let
    compareEntity entityAcc entity =
      collideWithEffect (\data ->
        { data | corp = Corporeal.setColor Color.green data.corp }
      ) entityAcc entity
  in
    List.foldl compareEntity entityAcc entities

updateEntities : Input -> App -> App
updateEntities input app =
  { app |
    entities = List.map (updateEntity input) app.entities
  }

updateEntity : Input -> Entity -> Entity
updateEntity input entity =
  case entity of
    Cursor data ->
      Cursor { data |
        space = Spatial.setPos input.mouse data.space
      }
    Turtle data ->
      Turtle { data |
        space = Spatial.setPos
          (Vec.x data.space.pos, (Vec.y data.space.pos) - 300 * input.delta)
          data.space
      }
    Labeler data ->
      Labeler data

---------------- View methods

view : (Int, Int) -> App -> Element
view (width, height) app =
  collage width height
  <| List.map viewEntity app.entities

viewEntity : Entity -> Form
viewEntity entity =
  case entity of
    Cursor data ->
      move data.space.pos <| cursorView data.corp.color
    Turtle data ->
      move data.space.pos <| turtleView data.corp.color
    Labeler data ->
      move data.space.pos <| labelerView data.corp.dim data.label

cursorView : Color -> Form
cursorView color =
  filled color <| ngon 3 10


turtleView : Color -> Form
turtleView color =
  filled color <| circle 10

labelerView : Vec.Vec -> Label -> Form
labelerView dim label =
  filled label.color <| (uncurry rect) dim
  --let
  --  labelLengths = List.map (\l -> (l, l.percent * dim.x)) labels
  --in
  --  group (List.map (\(label, len) -> filled label.color <| rect len dim.y) labelLengths)

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
