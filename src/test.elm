import Html exposing (..)

import Task exposing (Task)
import Signal
import Time
import Window

type alias Model =
  { count : Int
  , message : String
  }

init : Model
init =
  { count = 0
  , message = ""
  }


type Action = Tick | Shout String

mb = Signal.mailbox (Shout "")

empty = Task.succeed ()

msg addr i =
  Signal.send addr (Shout ("We've got "++(toString i)))

update address action model =
  case action of
    Tick ->
      let
          model' = { model | count = model.count+1 }
          task =
            if model'.count%5 == 0 then
              msg address model'.count
            else
              empty
      in
        (model', task)
    Shout s ->
      (Model model.count s, empty)



view size model =
  div []
    [ div [] [text <| toString model.count]
    , div [] [text model.message]
    , div [] [text <| toString size]
    ]


input =
  Signal.merge (Signal.map (always Tick) <| Time.fps 1) mb.signal

state_and_tasks =
  Signal.foldp (\a (m, _) -> update mb.address a m) (init, empty) input



main =
  Signal.map fst state_and_tasks
  |> Signal.map2 view Window.dimensions


port tasks : Signal (Task x ())
port tasks = Signal.map snd state_and_tasks
