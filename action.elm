module Action where

type Action =
    NoOp
  | Entity EntityAction
  | Egg EggAction

type EntityAction = Open | Explode

type EggAction = Explode
