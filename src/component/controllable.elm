module Component.Controllable exposing (..)

import Entity.Role
import Vec exposing (..)

type alias Model = {
  role : Entity.Role.Name
}

init : Entity.Role.Name -> Model
init role = {
    role = role
  }
