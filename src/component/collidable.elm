module Component.Collidable exposing (..)

import Entity.Role

type alias Model = {
    role: Entity.Role.Name
  , restitution: Float -- coef for elasticity of collision. set to 1 for elastic collision
  -- TODO should move to health and life component?
  , durability: Float -- when durability is overcome, the entity breaks.
  }

init : Entity.Role.Name -> Float -> Float -> Model
init role restitution durability = {
    role = role
  , restitution = restitution
  , durability = durability
  }

isRole : Entity.Role.Name -> Model -> Maybe Model
isRole role coll =
  if coll.role == role then
    Just coll
  else
    Nothing
