module List.Extra exposing (..)

pairs : List a -> List (a, a)
pairs elements =
  List.concat
  <| List.indexedMap (\index elem ->
      List.map2 (,) (List.repeat ((List.length elements) - 1 - index) elem) (List.drop (index + 1) elements)
    ) elements
