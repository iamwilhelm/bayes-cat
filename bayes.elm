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


-------------- Model methods

type Entity = CursorType Cursor
  | TurtleType Turtle
  | LabelerType Labeler

type alias Cursor =
  { space: Spatial.Spatial
  , corp: Corporeal.Corporeal
  }

initCursor =
  CursorType {
    space = Spatial.initSpatial
  , corp = Corporeal.initCorporeal
  }

type alias Turtle =
  { space: Spatial.Spatial
  , corp: Corporeal.Corporeal
  }

initTurtle =
  TurtleType {
    space = Spatial.createSpatial (0, 400)
  , corp = Corporeal.initCorporeal
  }

createTurtle : Vec.Vec -> Entity
createTurtle pos =
  TurtleType {
    space = Spatial.createSpatial pos
  , corp = Corporeal.initCorporeal
  }

type alias Label =
  { name : String
  , color : Color
  }

createLabel : String -> Color -> Label
createLabel name colour =
  { name = name, color = colour }

type alias Labeler =
  { space: Spatial.Spatial
  , corp: Corporeal.Corporeal
  , labels: List Label
  }

initLabeler : Entity
initLabeler =
  LabelerType {
    space = Spatial.createSpatial (0, 200)
  , corp = Corporeal.initCorporeal
  , labels = [createLabel "Cancer" Color.red]
  }

type alias App =
  { entities: List Entity
  , seed: Random.Seed
  }

initApp : App
initApp = {
    entities = [
      initCursor
    , initTurtle
    , initLabeler
    ]
  , seed = Random.initialSeed 0
  }

-------------- Update methods

update : Input -> App -> App
update input app =
  sourceTurtles app
    |> borderCollisionDetect input
    |> collisionDetect
    |> updateEntities input

sourceTurtles : App -> App
sourceTurtles app =
  let
    (shouldCreate, newSeed1) = Random.generate Random.bool app.seed
    (xPos, newSeed0) = Random.generate (Random.float -300 300) newSeed1
  in
    if List.length app.entities < 60 && shouldCreate == True then
      { app |
        entities = createTurtle (xPos, 400) :: app.entities
      , seed = newSeed0
      }
    else
      { app |
        seed = newSeed0
      }

borderCollisionDetect : Input -> App -> App
borderCollisionDetect input app =
  let
    withinBounds entity =
      case entity of
        CursorType _ ->
          True
        LabelerType _ ->
          True
        TurtleType data ->
          Vec.y data.space.pos > -400.0
          && Vec.x data.space.pos > -500.0
          && Vec.x data.space.pos < 500.0
  in
    { app |
      entities = List.filter withinBounds app.entities
    }

collisionDetect : App -> App
collisionDetect app =
  app

updateEntities : Input -> App -> App
updateEntities input app =
  { app |
    entities = List.map (updateEntity input) app.entities
  }

updateEntity : Input -> Entity -> Entity
updateEntity input entity =
  case entity of
    CursorType data ->
      CursorType { data |
        space = Spatial.setPos input.mouse data.space
      }
    TurtleType data ->
      TurtleType { data |
        space = Spatial.setPos
          (Vec.x data.space.pos, (Vec.y data.space.pos) - 300 * input.delta)
          data.space
      }
    LabelerType data ->
      LabelerType data

---------------- View methods

view : (Int, Int) -> App -> Element
view (width, height) app =
  collage width height
  <| List.map viewEntity app.entities

viewEntity : Entity -> Form
viewEntity entity =
  case entity of
    CursorType data ->
      move data.space.pos <| cursorView data.corp.color
    TurtleType data ->
      move data.space.pos <| turtleView data.corp.color
    LabelerType data ->
      move data.space.pos <| labelerView data.corp.dim data.labels

cursorView : Color -> Form
cursorView color =
  filled color <| ngon 3 10


turtleView : Color -> Form
turtleView color =
  filled color <| circle 10

labelerView : Vec.Vec -> List Label -> Form
labelerView dim labels =
  filled red <| circle 20
  --let
  --  labelLengths = List.map (\l -> (l, l.percent * dim.x)) labels
  --in
  --  group (List.map (\(label, len) -> filled label.color <| rect len dim.y) labelLengths)

-------------- Input methods

type alias Input =
  { mouse : (Float, Float)
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
    Signal.map2 Input
      (Signal.map2 screenToWorld Window.dimensions Mouse.position)
      delta

------------- Main functions

appState : Signal App
appState =
  Signal.foldp update initApp input

main : Signal Element
main =
  Signal.map2 view Window.dimensions appState
