module Day22.Main exposing (main)

import Browser
import Day22.Input exposing (rawInput)
import Dict exposing (Dict)
import Helpers.Helpers exposing (Delay(..), mapTuple, trigger)
import Html exposing (Html, div, text)


type Infection
    = Infected
    | Clean
    | Weakened
    | Flagged


type alias Position =
    ( Int, Int )


type alias Grid =
    Dict Position Infection


type Input
    = NotParsed
    | Parsed Position Grid


type Direction
    = Up
    | Down
    | Left
    | Right


type alias Model =
    { input : String
    , parsedInput : Input
    , firstPart : Maybe Int
    , secondPart : Maybe Int
    }


type Msg
    = Parse
    | RunFirst
    | RunSecond


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


mapChar : Char -> Maybe Infection
mapChar chr =
    case chr of
        '.' ->
            Just Clean

        '#' ->
            Just Infected

        'W' ->
            Just Weakened

        'F' ->
            Just Flagged

        _ ->
            Nothing


mapWithCoords : Int -> String -> List ( Position, Infection )
mapWithCoords y =
    List.indexedMap (\x chr -> ( ( x, negate y ), chr )) << List.filterMap mapChar << String.toList


parse : String -> ( Position, Grid )
parse input =
    let
        result =
            String.lines input
                |> List.indexedMap mapWithCoords
                |> List.concat
                |> Dict.fromList

        keys =
            Dict.keys result

        x =
            List.map Tuple.first keys
                |> List.maximum
                |> Maybe.withDefault 0
                |> (\b a -> a // b) 2

        y =
            List.map Tuple.second keys
                |> List.minimum
                |> Maybe.withDefault 0
                |> toFloat
                |> (\b a -> a / b) 2
                |> ceiling
    in
    ( ( x, y ), result )


turnLeft : Direction -> Direction
turnLeft direction =
    case direction of
        Up ->
            Left

        Down ->
            Right

        Left ->
            Down

        Right ->
            Up


turnRight : Direction -> Direction
turnRight direction =
    case direction of
        Up ->
            Right

        Down ->
            Left

        Left ->
            Up

        Right ->
            Down


turnBack : Direction -> Direction
turnBack direction =
    case direction of
        Up ->
            Down

        Down ->
            Up

        Left ->
            Right

        Right ->
            Left


moveForward : Position -> Direction -> Position
moveForward ( x, y ) direction =
    case direction of
        Up ->
            ( x, y + 1 )

        Down ->
            ( x, y - 1 )

        Left ->
            ( x - 1, y )

        Right ->
            ( x + 1, y )


runFirst : Grid -> Position -> Direction -> Int -> Int -> Int
runFirst grid position direction infections iterations =
    if iterations == 10000 then
        infections

    else
        let
            node =
                Dict.get position grid
                    |> Maybe.withDefault Clean

            ( newState, newDirection, newInfections ) =
                case node of
                    Clean ->
                        ( Infected, turnLeft direction, infections + 1 )

                    _ ->
                        ( Clean, turnRight direction, infections )

            newPosition =
                moveForward position newDirection

            newGrid =
                Dict.insert position newState grid
        in
        runFirst newGrid newPosition newDirection newInfections (iterations + 1)


runSecond : Grid -> Position -> Direction -> Int -> Int -> Int
runSecond grid position direction infections iterations =
    if iterations == 10000000 then
        infections

    else
        let
            node =
                Dict.get position grid
                    |> Maybe.withDefault Clean

            ( newState, newDirection, newInfections ) =
                case node of
                    Clean ->
                        ( Weakened, turnLeft direction, infections )

                    Weakened ->
                        ( Infected, direction, infections )

                    Infected ->
                        ( Flagged, turnRight direction, infections + 1 )

                    Flagged ->
                        ( Clean, turnBack direction, infections )

            newPosition =
                moveForward position newDirection

            newGrid =
                Dict.insert position newState grid
        in
        runSecond newGrid newPosition newDirection newInfections (iterations + 1)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Parse ->
            ( { model
                | parsedInput = mapTuple Parsed <| parse model.input
              }
            , trigger WithDelay RunFirst
            )

        RunFirst ->
            case model.parsedInput of
                NotParsed ->
                    ( model
                    , trigger WithDelay Parse
                    )

                Parsed position grid ->
                    ( { model
                        | firstPart = Just <| runFirst grid position Up 0 0
                      }
                    , trigger WithDelay RunSecond
                    )

        RunSecond ->
            case model.parsedInput of
                NotParsed ->
                    ( model
                    , trigger WithDelay Parse
                    )

                Parsed position grid ->
                    ( { model
                        | secondPart = Just <| runSecond grid position Up 0 0
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

            Parsed _ _ ->
                [ div [] [ text <| "Part 1: " ++ print model.firstPart ]
                , div [] [ text <| "Part 2: " ++ print model.secondPart ]
                ]
        )
