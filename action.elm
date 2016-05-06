module Action where

import Entity.Egg

type Action =
    NoOp
  | Egg Entity.Egg.Action
