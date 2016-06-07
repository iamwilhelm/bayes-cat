module Cmd.Extra exposing (..)

import Basics.Extra exposing (never)
import Task

msgToCmd : msg -> Cmd msg
msgToCmd msg =
  Task.perform never identity (Task.succeed msg)
