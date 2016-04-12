module Input where

import Time

type alias Input =
  { window: (Int, Int)
  , mouse : (Float, Float)
  , delta: Time.Time
  }
