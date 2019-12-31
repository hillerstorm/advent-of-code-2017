module Day12.Main exposing (countGroups, getGroup, main)

import Browser
import Day12.Input exposing (rawInput)
import Dict exposing (Dict)
import Helpers.Helpers exposing (Delay(..), prettyMaybe, trigger, unsafeGet)
import Html exposing (..)
import Set


type Input
    = NotParsed
    | Parsed (Dict.Dict Int (Set.Set Int))


type alias Model =
    { input : String
    , parsedInput : Input
    , firstPart : Maybe Int
    , secondPart : Maybe Int
    }


type Msg
    = Parse
    | Run


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { input = rawInput
      , parsedInput = NotParsed
      , firstPart = Nothing
      , secondPart = Nothing
      }
    , trigger NoDelay Parse
    )


toInt : String -> Maybe Int
toInt str =
    let
        cleaned =
            if String.endsWith "," str then
                String.dropRight 1 str

            else
                str
    in
    String.toInt cleaned


parseLine : String -> Maybe ( Int, Set.Set Int )
parseLine line =
    case String.split " " line of
        key :: "<->" :: first :: xs ->
            case toInt key of
                Just iKey ->
                    let
                        rest =
                            first
                                :: xs
                                |> List.filterMap toInt
                                |> List.filter ((/=) iKey)
                                |> Set.fromList
                    in
                    Just ( iKey, rest )

                Nothing ->
                    Nothing

        _ ->
            Nothing


parse : String -> Input
parse =
    Parsed << Dict.fromList << List.filterMap parseLine << String.lines


getGroup : Set.Set Int -> Dict.Dict Int (Set.Set Int) -> Set.Set Int -> Set.Set Int
getGroup set dict result =
    let
        toCheck =
            Set.diff set result
    in
    case Set.toList toCheck of
        [] ->
            result

        x :: xs ->
            let
                value =
                    unsafeGet x dict

                newResult =
                    getGroup value dict <| Set.insert x result
            in
            getGroup (Set.fromList xs) dict newResult


countGroups : Set.Set Int -> Dict.Dict Int (Set.Set Int) -> Int -> Int
countGroups toRemove dict cnt =
    let
        newDict =
            Dict.filter (\k v -> not <| Set.member k toRemove) dict
    in
    case Dict.keys newDict of
        [] ->
            cnt

        x :: _ ->
            let
                value =
                    unsafeGet x newDict

                newToRemove =
                    getGroup value newDict <| Set.singleton x
            in
            countGroups newToRemove newDict <| cnt + 1


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Parse ->
            ( { model
                | parsedInput = parse model.input
              }
            , trigger NoDelay Run
            )

        Run ->
            case model.parsedInput of
                NotParsed ->
                    ( model
                    , trigger NoDelay Parse
                    )

                Parsed dict ->
                    let
                        zeroValue =
                            unsafeGet 0 dict

                        groupZero =
                            getGroup zeroValue dict <| Set.singleton 0

                        secondPart =
                            countGroups groupZero dict 1
                    in
                    ( { model
                        | firstPart = Just <| Set.size groupZero
                        , secondPart = Just secondPart
                      }
                    , Cmd.none
                    )


view : Model -> Html msg
view model =
    div []
        (case model.parsedInput of
            NotParsed ->
                [ div [] [ text "Parsing..." ] ]

            Parsed input ->
                [ div [] [ text <| "Part 1: " ++ prettyMaybe model.firstPart ]
                , div [] [ text <| "Part 2: " ++ prettyMaybe model.secondPart ]
                ]
        )
