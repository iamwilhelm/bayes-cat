module Component.Label exposing (..)

import Color

type alias Model = {
    title : String
  , color : Color.Color
  }

init : String -> Color.Color -> Model
init title color = {
    title = title
  , color = color
  }
