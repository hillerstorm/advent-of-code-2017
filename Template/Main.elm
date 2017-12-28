module Template.Main exposing (main)

import Html exposing (..)
import Template.Input exposing (rawInput)
import Helpers.Helpers exposing (trigger, Delay(..))


type Input
    = NotParsed
    | Parsed String


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


parse : String -> Input
parse =
    Parsed << identity


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Parse ->
            { model
                | parsedInput = parse model.input
            }
                ! [ trigger NoDelay Run ]

        Run ->
            model ! []


print : Maybe Int -> String
print =
    Maybe.withDefault "Calculating..." << Maybe.map toString


view : Model -> Html msg
view model =
    div []
        (case model.parsedInput of
            NotParsed ->
                [ div [] [ text "Parsing..." ] ]

            Parsed input ->
                [ div [] [ text <| "Part 1: " ++ print model.firstPart ]
                , div [] [ text <| "Part 2: " ++ print model.secondPart ]
                ]
        )
