module Template.Main exposing (main)

import Html exposing (..)
import Template.Input exposing (parsedInput, Input)
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Run ->
            model ! []


view : Model -> Html msg
view model =
    div []
        [ div [] [ text <| "Part 1: " ++ toString model.firstPart ]
        , div [] [ text <| "Part 2: " ++ toString model.secondPart ]
        ]
