module Day09.Main exposing (main)

import Html exposing (..)
import Day09.Input exposing (parsedInput, Input, Thing(..))
import Helpers.Helpers exposing (trigger, Delay(..), prettyMaybe)


type alias Model =
    { input : Input
    , firstPart : Maybe Int
    , secondPart : Maybe Int
    }


type Msg
    = Run


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
    { input = parsedInput
    , firstPart = Nothing
    , secondPart = Nothing
    }
        ! [ trigger NoDelay Run ]


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
        Run ->
            { model
                | firstPart = Just <| getScore 1 model.input.children
                , secondPart = Just <| garbageLength model.input.children
            }
                ! []


view : Model -> Html msg
view model =
    div []
        [ div [] [ text <| "Part 1: " ++ prettyMaybe model.firstPart ]
        , div [] [ text <| "Part 2: " ++ prettyMaybe model.secondPart ]
        ]
