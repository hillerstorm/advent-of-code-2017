module Day09.Main exposing (main)

import Html exposing (..)
import Day09.Input exposing (rawInput)
import Helpers.Helpers exposing (trigger, Delay(..), prettyMaybe)


type alias Group =
    { children : List Thing
    }


type Thing
    = Thing Group
    | Garbage String


type Input
    = NotParsed
    | Parsed Group


type alias Model =
    { input : String
    , parsedInput : Input
    , firstPart : Maybe Int
    , secondPart : Maybe Int
    }


type Msg
    = Parse
    | Run


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }


init : ( Model, Cmd Msg )
init =
    { input = rawInput
    , parsedInput = NotParsed
    , firstPart = Nothing
    , secondPart = Nothing
    }
        ! [ trigger NoDelay Parse ]


parseGarbage : String -> String -> ( String, String )
parseGarbage garbage str =
    case String.uncons str of
        Just ( '!', xs ) ->
            case String.uncons xs of
                Just ( _, xs ) ->
                    parseGarbage garbage xs

                Nothing ->
                    Debug.crash "Invalid cancel"

        Just ( '>', xs ) ->
            ( garbage, xs )

        Just ( x, xs ) ->
            parseGarbage (garbage ++ String.fromChar x) xs

        Nothing ->
            Debug.crash "Invalid garbage"


parseGroup : List Thing -> String -> ( Group, String )
parseGroup children str =
    case String.uncons str of
        Just ( ',', xs ) ->
            parseGroup children xs

        Just ( '}', xs ) ->
            ( Group children, xs )

        Just ( '{', xs ) ->
            let
                ( grp, rest ) =
                    parseGroup [] xs
            in
                parseGroup (children ++ [ Thing grp ]) rest

        Just ( '<', xs ) ->
            let
                ( garbage, rest ) =
                    parseGarbage "" xs
            in
                parseGroup (children ++ [ Garbage garbage ]) rest

        _ ->
            Debug.crash "Invalid group"


parse : String -> Input
parse str =
    case String.uncons str of
        Just ( '{', xs ) ->
            let
                ( grp, rest ) =
                    parseGroup [] xs
            in
                if String.isEmpty rest then
                    Parsed grp
                else
                    Debug.crash "Invalid input"

        _ ->
            Debug.crash "Invalid input"


scoreThing : Int -> Thing -> Int
scoreThing score thing =
    case thing of
        Thing { children } ->
            getScore (score + 1) children

        Garbage _ ->
            0


getScore : Int -> List Thing -> Int
getScore score =
    ((+) score) << List.sum << List.map (scoreThing score)


countGarbage : Thing -> Int
countGarbage thing =
    case thing of
        Thing { children } ->
            garbageLength children

        Garbage garbage ->
            String.length garbage


garbageLength : List Thing -> Int
garbageLength =
    List.sum << List.map countGarbage


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Parse ->
            { model
                | parsedInput = parse model.input
            }
                ! [ trigger NoDelay Run ]

        Run ->
            case model.parsedInput of
                NotParsed ->
                    model ! []

                Parsed grp ->
                    { model
                        | firstPart = Just <| getScore 1 grp.children
                        , secondPart = Just <| garbageLength grp.children
                    }
                        ! []


view : Model -> Html msg
view model =
    div []
        [ div [] [ text <| "Part 1: " ++ prettyMaybe model.firstPart ]
        , div [] [ text <| "Part 2: " ++ prettyMaybe model.secondPart ]
        ]
