module Day21.Main exposing (main)

import Browser
import Day21.Input exposing (rawInput)
import Dict exposing (Dict)
import Helpers.Helpers exposing (Delay(..), flip, trigger, unique)
import Html exposing (Html, div, text)
import List.Extra
import String.Extra


type Input
    = NotParsed
    | Parsed (Dict String (List String))


type alias Model =
    { input : String
    , parsedInput : Input
    , state : String
    , iterations : Int
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
      , state =
            ".#./..#/###" |> splitPattern |> String.fromList
      , iterations = 0
      , firstPart = Nothing
      , secondPart = Nothing
      }
    , trigger NoDelay Parse
    )


isValidChar : Char -> Bool
isValidChar chr =
    chr == '.' || chr == '#'


splitPattern : String -> List Char
splitPattern =
    String.toList << String.filter isValidChar


parseRules : String -> Dict String (List String) -> Dict String (List String)
parseRules str rules =
    case String.words str of
        [ pattern, "=>", replacement ] ->
            case ( splitPattern pattern, splitPattern replacement ) of
                ( [ a, b, c, d ], [ e, f, g, h, i, j, k, l, m ] ) ->
                    let
                        repl =
                            [ [ e, f, g ]
                            , [ h, i, j ]
                            , [ k, l, m ]
                            ]
                                |> List.map String.fromList

                        patterns =
                            [ [ a, b, c, d ]
                            , [ c, a, d, b ]
                            , [ d, c, b, a ]
                            , [ b, d, a, c ]
                            , [ b, a, d, c ]
                            , [ c, d, a, b ]
                            , [ a, c, b, d ]
                            , [ d, b, c, a ]
                            ]
                                |> unique
                                |> List.map String.fromList
                    in
                    patterns
                        |> List.map (\xx -> ( xx, repl ))
                        |> Dict.fromList
                        |> Dict.union rules

                ( [ a, b, c, d, e, f, g, h, i ], [ j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y ] ) ->
                    let
                        repl =
                            [ [ j, k, l, m ]
                            , [ n, o, p, q ]
                            , [ r, s, t, u ]
                            , [ v, w, x, y ]
                            ]
                                |> List.map String.fromList

                        patterns =
                            [ [ a, b, c, d, e, f, g, h, i ]
                            , [ g, d, a, h, e, b, i, f, c ]
                            , [ i, h, g, f, e, d, c, b, a ]
                            , [ c, f, i, b, e, h, a, d, g ]
                            , [ c, b, a, f, e, d, i, h, g ]
                            , [ g, h, i, d, e, f, a, b, c ]
                            , [ a, d, g, b, e, h, c, f, i ]
                            , [ i, f, c, h, e, b, g, d, a ]
                            ]
                                |> unique
                                |> List.map String.fromList
                    in
                    patterns
                        |> List.map (\xx -> ( xx, repl ))
                        |> Dict.fromList
                        |> Dict.union rules

                _ ->
                    rules

        _ ->
            rules


parse : String -> Input
parse =
    Parsed << List.foldl parseRules Dict.empty << String.lines


mapWith : List String -> Int -> Int -> Int -> String
mapWith list width cols i =
    let
        dropped =
            List.drop i list

        a =
            List.head dropped
                |> Maybe.withDefault ""

        b =
            dropped
                |> List.drop cols
                |> List.head
                |> Maybe.withDefault ""

        c =
            if width == 3 then
                dropped
                    |> List.drop (cols + cols)
                    |> List.head
                    |> Maybe.withDefault ""

            else
                ""
    in
    a ++ b ++ c


pack : Dict.Dict String (List String) -> List Int -> Int -> Int -> Int -> List (List String) -> Int -> List String -> String
pack rules colRange num width cols result len list =
    if len < num then
        let
            newResult =
                List.filterMap (flip Dict.get rules << mapWith list width cols) colRange
                    |> (++) result

            newLen =
                len + cols

            newList =
                List.drop (cols * width) list
        in
        pack rules colRange num width cols newResult newLen newList

    else
        List.Extra.groupsOf cols result
            |> List.map List.Extra.transpose
            |> List.concat
            |> List.concat
            |> String.join ""


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

                Parsed rules ->
                    let
                        len =
                            String.length model.state

                        width =
                            if modBy 2 len == 0 then
                                2

                            else
                                3

                        num =
                            len // width // width

                        cols =
                            toFloat num
                                |> sqrt
                                |> floor

                        colRange =
                            List.range 0 (cols - 1)

                        state =
                            String.Extra.break width model.state
                                |> pack rules colRange num width cols [] 0

                        iterations =
                            model.iterations + 1

                        firstPart =
                            if iterations == 5 then
                                Just <| String.Extra.countOccurrences "#" state

                            else
                                model.firstPart

                        secondPart =
                            if iterations == 18 then
                                Just <| String.Extra.countOccurrences "#" state

                            else
                                model.secondPart

                        nextCmd =
                            case secondPart of
                                Just _ ->
                                    []

                                Nothing ->
                                    [ trigger (DelayWithMs 1) Run ]
                    in
                    ( { model
                        | state = state
                        , iterations = iterations
                        , firstPart = firstPart
                        , secondPart = secondPart
                      }
                    , Cmd.batch nextCmd
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
