module Component.Renderable exposing (..)

import Entity.Role

type alias Model = {
  role : Entity.Role.Name
}

init : Entity.Role.Name -> Model
init role = {
    role = role
  }
