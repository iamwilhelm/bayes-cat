module Component.Controllable exposing (..)

import Entity.Role

type alias Model = {
  role : Entity.Role.Name
}

init : Entity.Role.Name -> Model
init role = {
    role = role
  }

isRole : Entity.Role.Name -> Model -> Maybe Model
isRole role ctrl =
  if ctrl.role == role then
    Just ctrl
  else
    Nothing
