module Day09.Main exposing (main)

import Html exposing (..)
import Day09.Input exposing (parsedInput, Input, Thing(..))
import Helpers.Helpers exposing (trigger, Delay(..))


type alias Model =
    { input : Input
    , firstPart : Int
    , secondPart : Int
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
    , firstPart = 0
    , secondPart = 0
    }
        ! [ trigger NoDelay Run ]


scoreThing : Int -> Thing -> Int
scoreThing score thing =
    case thing of
        Thing grp ->
            getScore grp <| score + 1

        Garbage _ ->
            0


getScore : Input -> Int -> Int
getScore grp score =
    case grp.children of
        [] ->
            score

        xs ->
            let
                childrenScore =
                    List.sum <| List.map (scoreThing score) xs
            in
                score + childrenScore


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Run ->
            { model
                | firstPart = getScore model.input 1
            }
                ! []


view : Model -> Html msg
view model =
    div []
        [ div [] [ text <| "Part 1: " ++ toString model.firstPart ]
        , div [] [ text <| "Part 2: " ++ toString model.secondPart ]
        ]
