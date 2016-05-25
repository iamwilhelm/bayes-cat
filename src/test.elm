import Html exposing (..)

import Task exposing (Task)
import Time
import Window

import Debug

pairs : List a -> List (a, a)
pairs elements =
  List.concat
  <| List.indexedMap (\index elem ->
      let
        rest = List.drop (index + 1) elements
        restLen = (List.length elements) - 1 - index
      in
        List.map2 (,) (List.repeat restLen elem) rest
    ) elements

main =
  let
    _ = Debug.log "result: " <| pairs [1]
  in
    div [] [text "hello"]
