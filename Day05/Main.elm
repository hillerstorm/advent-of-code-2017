module Day05.Main exposing (main)

import Html exposing (..)
import List.Extra exposing (getAt, setAt)
import Day05.Input exposing (parsedInput)
import Helpers.Helpers exposing (trigger, Delay(..))


type alias Model =
    { input : List Int
    , current : List Int
    , idx : Int
    , steps : Int
    }


type Msg
    = Jump


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
    , current = parsedInput
    , idx = 0
    , steps = 0
    }
        ! [ trigger NoDelay Jump ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Jump ->
            case getAt model.idx model.current of
                Just x ->
                    case setAt model.idx (x + 1) model.current of
                        Just y ->
                            { model
                                | current = y
                                , idx = model.idx + x
                                , steps = model.steps + 1
                            }
                                ! [ trigger NoDelay Jump ]

                        Nothing ->
                            { model | steps = model.steps + 1 } ! []

                Nothing ->
                    model ! []


view : Model -> Html msg
view model =
    div []
        [ div [] [ text <| "Part 1: " ++ toString model.steps ]
        ]
