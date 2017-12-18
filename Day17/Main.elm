module Day17.Main exposing (main)

import Html exposing (..)
import Helpers.Helpers exposing (trigger, Delay(..), prettyMaybe)
import List.Extra


input : Int
input =
    376


type alias Model =
    { input : Int
    , state : List Int
    , index : Int
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
    { input = input
    , state = []
    , index = 0
    , firstPart = Nothing
    , secondPart = Nothing
    }
        ! [ trigger NoDelay Run ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Run ->
            let
                len =
                    List.length model.state

                idx =
                    if len == 0 then
                        0
                    else
                        (model.index + model.input) % len

                ( l, r ) =
                    List.Extra.splitAt (idx + 1) model.state

                newState =
                    l ++ [ len ] ++ r

                newIndex =
                    if len == 0 then
                        idx
                    else
                        idx + 1

                firstPart =
                    if len == 2017 then
                        case List.head r of
                            Just val ->
                                Just val

                            Nothing ->
                                List.head l
                    else
                        Nothing

                nextCmd =
                    case firstPart of
                        Just _ ->
                            []

                        Nothing ->
                            [ trigger NoDelay Run ]
            in
                { model
                    | state = newState
                    , index = newIndex
                    , firstPart = firstPart
                }
                    ! nextCmd


view : Model -> Html msg
view model =
    div []
        [ div [] [ text <| "Part 1: " ++ prettyMaybe model.firstPart ]
        , div [] [ text <| "Part 2: " ++ prettyMaybe model.secondPart ]
        ]
