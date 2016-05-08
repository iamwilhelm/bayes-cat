module Action where

import Entity.Egg
import Entity.EntityList

type Action =
    NoOp
  | Egg Entity.Egg.Action
  | EntityList Entity.EntityList.Action
