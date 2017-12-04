module Helpers.Helpers exposing (trigger, Delay(..), count, unique)

import Task exposing (..)
import Process exposing (sleep)
import Time exposing (millisecond)
import Set


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


count : (a -> Bool) -> List a -> Int
count f =
    List.length << List.filter f


unique : List comparable -> List comparable
unique =
    Set.toList << Set.fromList
