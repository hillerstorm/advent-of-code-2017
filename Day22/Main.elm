module Day22.Main exposing (main)

import Html exposing (..)
import Day22.Input exposing (rawInput)
import Helpers.Helpers exposing (trigger, Delay(..))
import Dict.LLRB as Dict


type Infection
    = Infected
    | Clean
    | Weakened
    | Flagged


type alias Position =
    ( Int, Int )


type Input
    = NotParsed
    | Parsed ( Position, Dict.Dict Position Infection )


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
        ! [ trigger WithDelay Parse ]


mapChar : Char -> Infection
mapChar chr =
    case chr of
        '.' ->
            Clean

        '#' ->
            Infected

        'W' ->
            Weakened

        'F' ->
            Flagged

        _ ->
            Debug.crash "Invalid"


mapWithCoords : Int -> String -> List ( Position, Infection )
mapWithCoords y =
    List.indexedMap (\x chr -> ( ( x, negate y ), mapChar chr )) << String.toList


parse : String -> ( Position, Dict.Dict Position Infection )
parse input =
    let
        result =
            input
                |> String.lines
                |> List.indexedMap mapWithCoords
                |> List.concat
                |> Dict.fromList

        keys =
            Dict.keys result

        x =
            keys
                |> List.map Tuple.first
                |> List.maximum
                |> Maybe.withDefault 0
                |> (flip (//)) 2

        y =
            keys
                |> List.map Tuple.second
                |> List.minimum
                |> Maybe.withDefault 0
                |> toFloat
                |> (flip (/)) 2
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


runFirst : Dict.Dict Position Infection -> Position -> Direction -> Int -> Int -> Int
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
            runFirst newGrid newPosition newDirection newInfections <| iterations + 1


runSecond : Dict.Dict Position Infection -> Position -> Direction -> Int -> Int -> Int
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
            runSecond newGrid newPosition newDirection newInfections <| iterations + 1


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Parse ->
            let
                ( position, parsedInput ) =
                    parse model.input
            in
                { model
                    | parsedInput = Parsed ( position, parsedInput )
                }
                    ! [ trigger WithDelay RunFirst ]

        RunFirst ->
            case model.parsedInput of
                NotParsed ->
                    model ! [ trigger WithDelay Parse ]

                Parsed ( position, grid ) ->
                    { model
                        | firstPart = Just <| runFirst grid position Up 0 0
                    }
                        ! [ trigger WithDelay RunSecond ]

        RunSecond ->
            case model.parsedInput of
                NotParsed ->
                    model ! [ trigger WithDelay Parse ]

                Parsed ( position, grid ) ->
                    { model
                        | secondPart = Just <| runSecond grid position Up 0 0
                    }
                        ! []


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
