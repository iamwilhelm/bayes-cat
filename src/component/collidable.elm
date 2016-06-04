module Component.Collidable exposing (..)

import Entity.Role

type alias Model = {
    role: Entity.Role.Name
  , restitution: Float -- coef for elasticity of collision. set to 1 for elastic collision
  , isColliding: Bool
  }

init : Entity.Role.Name -> Float -> Float -> Model
init role restitution durability = {
    role = role
  , restitution = restitution
  , isColliding = False
  }

isRole : Entity.Role.Name -> Model -> Maybe Model
isRole role coll =
  if coll.role == role then
    Just coll
  else
    Nothing
