module Template.Main exposing (main)

import Browser
import Helpers.Helpers exposing (Delay(..), trigger)
import Html exposing (Html, div, text)
import Template.Input exposing (rawInput)


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
    , trigger WithDelay Parse
    )


parse : String -> Input
parse =
    Parsed << identity


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Parse ->
            ( { model
                | parsedInput = parse model.input
              }
            , trigger WithDelay Run
            )

        Run ->
            case model.parsedInput of
                NotParsed ->
                    ( model
                    , trigger WithDelay Parse
                    )

                Parsed _ ->
                    ( model
                    , Cmd.none
                    )


print : Maybe Int -> String
print =
    Maybe.withDefault "Calculating..." << Maybe.map String.fromInt


view : Model -> Html msg
view model =
    div []
        (case model.parsedInput of
            NotParsed ->
                [ div [] [ text "Parsing..." ] ]

            Parsed _ ->
                [ div [] [ text <| "Part 1: " ++ print model.firstPart ]
                , div [] [ text <| "Part 2: " ++ print model.secondPart ]
                ]
        )
