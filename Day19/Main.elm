module Day19.Main exposing (main)

import Browser
import Char
import Day19.Input exposing (rawInput)
import Helpers.Helpers exposing (Delay(..), trigger)
import Html exposing (Html, div, text)
import List.Extra


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
      , direction = Down
      , pos = ( 0, 0 )
      , letters = ""
      , lastChar = '|'
      , steps = 0
      , firstPart = Nothing
      , secondPart = Nothing
      }
    , trigger NoDelay Parse
    )


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
            List.map (String.padRight width ' ') lines
                |> String.concat
                |> String.toList
    in
    ( width, padded )


type alias NextDirectionValues =
    { idx1 : Int
    , idx2 : Int
    , chr : Char
    , first : Direction
    , second : Direction
    }


getDirection : String -> Int -> Direction -> Int -> Int -> Maybe Direction
getDirection grid width direction x y =
    let
        nextDirValues =
            if direction == Up || direction == Down then
                let
                    idx1 =
                        (x - 1) + width * y

                    idx2 =
                        (x + 1) + width * y
                in
                NextDirectionValues idx1 idx2 '-' Left Right

            else
                let
                    idx1 =
                        x + width * (y - 1)

                    idx2 =
                        x + width * (y + 1)
                in
                NextDirectionValues idx1 idx2 '|' Up Down

        aVal =
            String.dropLeft nextDirValues.idx1 grid
                |> String.uncons
                |> Maybe.map Tuple.first

        bVal =
            String.dropLeft nextDirValues.idx2 grid
                |> String.uncons
                |> Maybe.map Tuple.first
    in
    case ( aVal, bVal ) of
        ( Just a, Just b ) ->
            let
                aCode =
                    Char.toCode a

                bCode =
                    Char.toCode b
            in
            if a == nextDirValues.chr || a == '+' || (aCode >= 65 && aCode <= 90) then
                Just nextDirValues.first

            else if b == nextDirValues.chr || b == '+' || (bCode >= 65 && bCode <= 90) then
                Just nextDirValues.second

            else
                Nothing

        _ ->
            Nothing


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Parse ->
            let
                ( width, parsedInput ) =
                    parse model.input

                index =
                    List.take width parsedInput
                        |> List.Extra.elemIndex '|'
            in
            case index of
                Just idx ->
                    ( { model
                        | parsedInput =
                            Parsed
                                { grid = String.fromList parsedInput
                                , width = width
                                }
                        , pos = ( idx, 0 )
                      }
                    , trigger NoDelay Run
                    )

                Nothing ->
                    ( model, Cmd.none )

        Run ->
            case model.parsedInput of
                NotParsed ->
                    ( model
                    , trigger WithDelay Parse
                    )

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
                            String.dropLeft index grid
                                |> String.uncons
                                |> Maybe.map Tuple.first
                                |> Maybe.withDefault model.lastChar

                        lastCharCode =
                            Char.toCode lastChar

                        letters =
                            if lastCharCode >= 65 && lastCharCode <= 90 then
                                model.letters ++ String.fromChar lastChar

                            else
                                model.letters

                        direction =
                            if lastChar == '+' then
                                getDirection grid width model.direction newX newY
                                    |> Maybe.withDefault model.direction

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
                                    [ trigger NoDelay Run ]
                    in
                    ( { model
                        | direction = direction
                        , letters = letters
                        , lastChar = lastChar
                        , pos = ( newX, newY )
                        , steps = steps
                        , firstPart = firstPart
                        , secondPart = secondPart
                      }
                    , Cmd.batch nextCmd
                    )


print : Maybe Int -> String
print =
    printStr << Maybe.map String.fromInt


printStr : Maybe String -> String
printStr =
    Maybe.withDefault "Calculating..."


view : Model -> Html msg
view model =
    div []
        (case model.parsedInput of
            NotParsed ->
                [ div [] [ text "Parsing..." ] ]

            Parsed _ ->
                [ div [] [ text <| "Part 1: " ++ printStr model.firstPart ]
                , div [] [ text <| "Part 2: " ++ print model.secondPart ]
                ]
        )
