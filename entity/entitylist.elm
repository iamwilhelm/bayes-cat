module Entity.EntityList where

import Random

import Entity.Egg
import Entity.Pointer

-- EntityList model is a functor

type alias Model = {
    entities : List (ID, Entity.Egg.Egg)
  , nextID : ID
  , seed : Random.Seed
  }

type alias ID = Int

init : Model
init = {
    entities = [
      -- TODO add pointer somewhere else. It should start off empty
      (0, Entity.Pointer.init)
    ]
  , nextID = 1
  , seed = Random.initialSeed 0
  }

type Action
  = Insert
  | Remove ID

reduce : Action -> Model -> Model
reduce action model =
  case action of
    Insert ->
      let
        (shouldCreate, seed1) = Random.generate Random.bool model.seed
        (xPos, seed2) = Random.generate (Random.float -300 300) seed1
      in
        if List.length model.entities < 5 && shouldCreate == True then
          { model |
            entities = (model.nextID, Entity.Egg.create (xPos, 300) (0, -300)) :: model.entities
          , nextID = model.nextID + 1
          , seed = seed2
          }
        else
          model
    Remove id ->
      { model |
        entities = List.filter (\(entityId, _) -> entityId /= id) model.entities
      }

map : (Entity.Egg.Egg -> Entity.Egg.Egg) -> Model -> Model
map func model =
  { model |
    entities = List.map (\(id, entity) -> (id, func entity)) model.entities
  }
