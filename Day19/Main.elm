module Day19.Main exposing (main)

import Html exposing (..)
import Day19.Input exposing (rawInput)
import Helpers.Helpers exposing (trigger, Delay(..))
import Char
import List.Extra


type alias Position =
    ( Int, Int )


type alias ParsedInput =
    { grid : String
    , width : Int
    }


type Input
    = NotParsed
    | Parsed ParsedInput


type Direction
    = Up
    | Down
    | Left
    | Right


type alias Model =
    { input : String
    , parsedInput : Input
    , direction : Direction
    , pos : ( Int, Int )
    , letters : String
    , lastChar : Char
    , steps : Int
    , firstPart : Maybe String
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
    , direction = Down
    , pos = ( 0, 0 )
    , letters = ""
    , lastChar = '|'
    , steps = 0
    , firstPart = Nothing
    , secondPart = Nothing
    }
        ! [ trigger NoDelay Parse ]


parse : String -> ( Int, List Char )
parse input =
    let
        lines =
            String.lines input

        width =
            List.Extra.maximumBy String.length lines
                |> Maybe.map String.length
                |> Maybe.withDefault 0
                |> (+) 1

        padded =
            lines
                |> List.map (String.padRight width ' ')
                |> String.concat
                |> String.toList
    in
        ( width, padded )


getDirection : String -> Int -> Direction -> Int -> Int -> Direction
getDirection grid width direction x y =
    let
        ( idx1, idx2, chr, first, second ) =
            if direction == Up || direction == Down then
                let
                    idx1 =
                        (x - 1) + width * y

                    idx2 =
                        (x + 1) + width * y
                in
                    ( idx1, idx2, '-', Left, Right )
            else
                let
                    idx1 =
                        x + width * (y - 1)

                    idx2 =
                        x + width * (y + 1)
                in
                    ( idx1, idx2, '|', Up, Down )
    in
        case ( String.dropLeft idx1 grid |> String.uncons, String.dropLeft idx2 grid |> String.uncons ) of
            ( Just ( a, _ ), Just ( b, _ ) ) ->
                let
                    aCode =
                        Char.toCode a

                    bCode =
                        Char.toCode b
                in
                    if a == chr || a == '+' || (aCode >= 65 && aCode <= 90) then
                        first
                    else if b == chr || b == '+' || (bCode >= 65 && bCode <= 90) then
                        second
                    else
                        Debug.crash "Invalid direction"

            _ ->
                Debug.crash "Invalid direction"


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Parse ->
            let
                ( width, parsedInput ) =
                    parse model.input

                index =
                    parsedInput
                        |> List.take width
                        |> List.Extra.elemIndex '|'
            in
                case index of
                    Just idx ->
                        { model
                            | parsedInput =
                                Parsed
                                    { grid = String.fromList parsedInput
                                    , width = width
                                    }
                            , pos = ( idx, 0 )
                        }
                            ! [ trigger WithDelay Run ]

                    Nothing ->
                        Debug.crash "Invalid input"

        Run ->
            case model.parsedInput of
                NotParsed ->
                    model ! [ trigger WithDelay Parse ]

                Parsed { grid, width } ->
                    let
                        ( x, y ) =
                            model.pos

                        ( newX, newY ) =
                            case model.direction of
                                Up ->
                                    ( x, y - 1 )

                                Down ->
                                    ( x, y + 1 )

                                Left ->
                                    ( x - 1, y )

                                Right ->
                                    ( x + 1, y )

                        index =
                            newX + width * newY

                        lastChar =
                            case String.dropLeft index grid |> String.uncons of
                                Just ( char, _ ) ->
                                    char

                                Nothing ->
                                    Debug.crash "Invalid move"

                        lastCharCode =
                            Char.toCode lastChar

                        letters =
                            if lastCharCode >= 65 && lastCharCode <= 90 then
                                model.letters ++ (String.fromChar lastChar)
                            else
                                model.letters

                        direction =
                            if lastChar == '+' then
                                getDirection grid width model.direction newX newY
                            else
                                model.direction

                        firstPart =
                            if lastChar == ' ' then
                                Just letters
                            else
                                Nothing

                        steps =
                            model.steps + 1

                        secondPart =
                            Maybe.map (always steps) firstPart

                        nextCmd =
                            case firstPart of
                                Just _ ->
                                    []

                                Nothing ->
                                    [ trigger (DelayWithMs 0) Run ]
                    in
                        { model
                            | direction = direction
                            , letters = letters
                            , lastChar = lastChar
                            , pos = ( newX, newY )
                            , steps = steps
                            , firstPart = firstPart
                            , secondPart = secondPart
                        }
                            ! nextCmd


print : Maybe a -> String
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
