module Component.KeyboardControl exposing (..)

type Msg =
    Up
  | Down
  | Left
  | Right

type alias Model = {
    foo : Int
}

init : Model
init =
  { foo = 1 }
