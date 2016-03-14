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
        entities = createTurtle xPos :: app.entities
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
        Cursor _ ->
          True
        Turtle _ ->
          (getPos entity).y > -400.0
          && (getPos entity).x > -500.0
          && (getPos entity).x < 500.0
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
    Cursor object ->
      Cursor { object |
        pos = { x = fst input.mouse, y = snd input.mouse }
      }
    Turtle object ->
      Turtle { object |
        pos = { x = object.pos.x, y = object.pos.y - 300 * input.delta }
      }

-------------- Model methods

type alias App =
  { entities: List Entity
  , seed: Random.Seed
  }

initApp : App
initApp = {
    entities = [
      Cursor {
        pos = { x = 0.0, y = 400.0 }
      , vel = { x = 0.0, y = -10.0 }
      , acc = { x = 0.0, y = -10.0 }
      }
    , Turtle {
        pos = { x = 0.0, y = 400.0 }
      , vel = { x = 0.0, y = -10.0 }
      , acc = { x = 0.0, y = -10.0 }
      }
    ]
  , seed = Random.initialSeed 0
  }

type Entity = Cursor Object | Turtle Object

type alias Object =
  {
    pos : Vec2
  , vel : Vec2
  , acc : Vec2
  }

type alias Vec2 =
  { x : Float, y : Float }

createTurtle : Float -> Entity
createTurtle xPos =
  Turtle {
    pos = { x = xPos , y = 400.0 }
  , vel = { x = 0.0, y = -10.0 }
  , acc = { x = 0.0, y = -10.0 }
  }

getPos : Entity -> Vec2
getPos entity =
  case entity of
    Cursor object ->
      object.pos
    Turtle object ->
      object.pos


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