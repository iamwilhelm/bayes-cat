import Html exposing (..)
import Html.Attributes exposing (..)
import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Window
import Mouse
import Time exposing (..)
import List

view : (Int, Int) -> App -> Element
view (width, height) app =
  collage width height
  <| List.map viewEntity app.entities

viewEntity : Entity -> Form
viewEntity entity =
  case entity of
    Cursor object ->
      move (object.pos.x, object.pos.y) kitty
    Turtle object ->
      move (object.pos.x, object.pos.y) doggy

kitty : Form
kitty =
  traced (solid blue) square

doggy : Form
doggy =
  traced (solid red) square

square : Path
square =
  path [ (10, 10), (10, -10), (-10, -10), (-10, 10), (10, 10) ]

-------------- Update methods

update : Input -> App -> App
update input app =
  { app |
    entities = List.map (updateEntity input) app.entities
  }

updateEntity : Input -> Entity -> Entity
updateEntity input entity =
  case entity of
    Cursor object ->
      Cursor { object |
        pos = { x = fst input.mouse, y = snd input.mouse }
      }
    Turtle object ->
      Turtle { object |
        pos = { x = object.pos.x, y = object.pos.y - 2 }
      }

-------------- Model methods

type alias App =
  { entities: List Entity
  }

initApp : App
initApp = {
    entities = [
      Cursor { pos = { x = 0.0, y = 0.0 } }
    , Turtle { pos = { x = 0.0, y = 0.0 } }
    ]
  }

type Entity =
  Cursor Object
  | Turtle Object

type alias Object =
  {
    pos : Vec2
  }

type alias Vec2 =
  { x : Float, y : Float }


-------------- Input methods

type alias Input =
  { mouse : (Float, Float)
  , delta: Time }

delta : Signal Time
delta = Signal.map inSeconds (fps 25)

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
