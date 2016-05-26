module Component.Collidable exposing (..)

import Entity.Role

type alias Model = {
    role: Entity.Role.Name
  }

init : Entity.Role.Name -> Model
init role = {
    role = role
  }

isRole : Entity.Role.Name -> Model -> Maybe Model
isRole role coll =
  if coll.role == role then
    Just coll
  else
    Nothing
