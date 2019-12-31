module Helpers.Helpers exposing
    ( Delay(..)
    , count
    , flip
    , mapTuple
    , prettyMaybe
    , sortDesc
    , trigger
    , unique
    , unsafeGet
    , unsafeToInt
    )

import Dict exposing (Dict)
import Process exposing (sleep)
import Set
import Task exposing (..)


type Delay
    = NoDelay
    | WithDelay
    | DelayWithMs Float


flip : (a -> b -> c) -> b -> a -> c
flip fn b a =
    fn a b


trigger : Delay -> msg -> Cmd msg
trigger delay msg =
    case delay of
        WithDelay ->
            Process.sleep 5 |> Task.perform (\_ -> msg)

        DelayWithMs d ->
            Process.sleep d |> Task.perform (\_ -> msg)

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
            Debug.toString x

        Nothing ->
            Debug.toString aMaybe


unsafeGet : comparable -> Dict comparable v -> v
unsafeGet key dict =
    case Dict.get key dict of
        Just x ->
            x

        Nothing ->
            Debug.todo "Key not found..."


unsafeToInt : String -> Int
unsafeToInt str =
    case String.toInt str of
        Just val ->
            val

        Nothing ->
            Debug.todo "Invalid number"


mapTuple : (a -> b -> c) -> ( a, b ) -> c
mapTuple f ( a, b ) =
    f a b


sortDesc : comparable -> comparable -> Order
sortDesc a b =
    case compare a b of
        LT ->
            GT

        GT ->
            LT

        EQ ->
            EQ
