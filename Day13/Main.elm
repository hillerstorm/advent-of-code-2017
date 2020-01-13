module Day13.Main exposing (main)

import Browser
import Day13.Input exposing (rawInput)
import Helpers.Helpers exposing (Delay(..), trigger)
import Html exposing (Html, div, text)


type Input
    = NotParsed
    | Parsed (List ( Int, Int ))


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


toTuple : String -> Maybe ( Int, Int )
toTuple str =
    case String.split ": " str of
        [ k, v ] ->
            Maybe.map2 Tuple.pair (String.toInt k) (String.toInt v)

        _ ->
            Nothing


parse : String -> Input
parse =
    Parsed << List.filterMap toTuple << String.lines


getIdx : Int -> Int -> Int
getIdx key value =
    let
        len =
            (value * 2) - 2

        idx =
            modBy len key
    in
    if idx < value then
        idx

    else
        len - idx


getSeverity : ( Int, Int ) -> Int
getSeverity ( key, value ) =
    if getIdx key value == 0 then
        key * value

    else
        0


isElsewhere : Int -> ( Int, Int ) -> Bool
isElsewhere offset ( key, value ) =
    getIdx (key + offset) value > 0


findOffset : List ( Int, Int ) -> Int -> Int
findOffset layers offset =
    if List.all (isElsewhere offset) layers then
        offset

    else
        findOffset layers (offset + 1)


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

                Parsed layers ->
                    let
                        severity =
                            layers
                                |> List.map getSeverity
                                |> List.sum

                        minOffset =
                            findOffset layers 0
                    in
                    ( { model
                        | firstPart = Just severity
                        , secondPart = Just minOffset
                      }
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
