module Input exposing (..)

import Time

type alias Model =
  { window: (Int, Int)
  , mouse : (Float, Float)
  , delta: Time.Time
  }
