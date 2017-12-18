module Helpers.Helpers
    exposing
        ( trigger
        , Delay(..)
        , count
        , unique
        , prettyMaybe
        , unsafeGet
        , unsafeToInt
        , mapTuple
        )

import Task exposing (..)
import Process exposing (sleep)
import Time exposing (millisecond)
import Set
import Dict.LLRB as Dict


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


prettyMaybe : Maybe a -> String
prettyMaybe aMaybe =
    case aMaybe of
        Just x ->
            toString x

        Nothing ->
            toString aMaybe


unsafeGet : comparable -> Dict.Dict comparable v -> v
unsafeGet key dict =
    case Dict.get key dict of
        Just x ->
            x

        Nothing ->
            Debug.crash "Key not found..."


unsafeToInt : String -> Int
unsafeToInt str =
    case String.toInt str of
        Ok val ->
            val

        Err _ ->
            Debug.crash "Invalid number"


mapTuple : (a -> b -> c) -> ( a, b ) -> c
mapTuple f ( a, b ) =
    f a b
