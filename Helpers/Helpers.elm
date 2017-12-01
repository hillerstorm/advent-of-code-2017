module Helpers.Helpers exposing (trigger, Delay(..))

import Task exposing (..)
import Process exposing (sleep)
import Time exposing (millisecond)


type Delay
    = NoDelay
    | WithDelay
    | DelayWithMs Float


trigger : Delay -> msg -> Cmd msg
trigger delay msg =
    case delay of
        WithDelay ->
            Process.sleep (5 * millisecond) |> Task.perform (\_ -> msg)

        DelayWithMs d ->
            Process.sleep (d * millisecond) |> Task.perform (\_ -> msg)

        NoDelay ->
            Task.perform identity <| Task.succeed msg
