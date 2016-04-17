module Action where

type EntityAction = Open | Explode
type Action = NoOp | Entity EntityAction
