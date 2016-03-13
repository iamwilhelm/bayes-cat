import Html exposing (..)
import Html.Attributes exposing (..)
import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Window
import Mouse
import Time exposing (..)

view : (Int, Int) -> App -> Element
view (width, height) app =
  collage width height
    [ move (app.x, app.y) kitty
    ]

kitty : Form
kitty =
  traced (solid blue) square

square : Path
square =
  path [ (10, 10), (10, -10), (-10, -10), (-10, 10), (10, 10) ]

-------------- Update methods

update : Input -> App -> App
update input app =
  { app |
    x = fst input.mouse
  , y = snd input.mouse
  }

-------------- Model methods

type alias App =
  { x : Float
  , y : Float
  }

initApp : App
initApp = {
    x = 0.0
  , y = 0.0
  }

-------------- Input methods

type alias Input =
  { mouse : (Float, Float)
  , delta: Time }

delta : Signal Time
delta = Signal.map inSeconds (fps 15)


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
