module Day21.Main exposing (main)

import Html exposing (..)
import Day21.Input exposing (rawInput)
import Helpers.Helpers exposing (trigger, Delay(..), unique, count, unsafeGet)
import List.Extra
import String.Extra
import Dict.LLRB as Dict


getUnsafe : Dict.Dict comparable v -> comparable -> v
getUnsafe =
    flip unsafeGet


type Input
    = NotParsed
    | Parsed (Dict.Dict String (List String))


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
    , state =
        ".#./..#/###" |> splitPattern |> String.fromList
    , iterations = 0
    , firstPart = Nothing
    , secondPart = Nothing
    }
        ! [ trigger NoDelay Parse ]


isValidChar : Char -> Bool
isValidChar chr =
    chr == '.' || chr == '#'


splitPattern : String -> List Char
splitPattern =
    String.toList << String.filter isValidChar


parseRules : String -> Dict.Dict String (List String) -> Dict.Dict String (List String)
parseRules str rules =
    case String.words str of
        [ pattern, "=>", replacement ] ->
            case ( splitPattern pattern, splitPattern replacement ) of
                ( [ a, b, c, d ], [ e, f, g, h, i, j, k, l, m ] ) ->
                    let
                        repl =
                            [ e :: f :: [ g ]
                            , h :: i :: [ j ]
                            , k :: l :: [ m ]
                            ]
                                |> List.map (String.fromList)

                        patterns =
                            [ a :: b :: c :: [ d ]
                            , c :: a :: d :: [ b ]
                            , d :: c :: b :: [ a ]
                            , b :: d :: a :: [ c ]
                            , b :: a :: d :: [ c ]
                            , c :: d :: a :: [ b ]
                            , a :: c :: b :: [ d ]
                            , d :: b :: c :: [ a ]
                            ]
                                |> unique
                                |> List.map (String.fromList)
                    in
                        patterns
                            |> List.map ((flip (,)) repl)
                            |> Dict.fromList
                            |> Dict.union rules

                ( [ a, b, c, d, e, f, g, h, i ], [ j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y ] ) ->
                    let
                        repl =
                            [ j :: k :: l :: [ m ]
                            , n :: o :: p :: [ q ]
                            , r :: s :: t :: [ u ]
                            , v :: w :: x :: [ y ]
                            ]
                                |> List.map (String.fromList)

                        patterns =
                            [ a :: b :: c :: d :: e :: f :: g :: h :: [ i ]
                            , g :: d :: a :: h :: e :: b :: i :: f :: [ c ]
                            , i :: h :: g :: f :: e :: d :: c :: b :: [ a ]
                            , c :: f :: i :: b :: e :: h :: a :: d :: [ g ]
                            , c :: b :: a :: f :: e :: d :: i :: h :: [ g ]
                            , g :: h :: i :: d :: e :: f :: a :: b :: [ c ]
                            , a :: d :: g :: b :: e :: h :: c :: f :: [ i ]
                            , i :: f :: c :: h :: e :: b :: g :: d :: [ a ]
                            ]
                                |> unique
                                |> List.map (String.fromList)
                    in
                        patterns
                            |> List.map ((flip (,)) repl)
                            |> Dict.fromList
                            |> Dict.union rules

                _ ->
                    Debug.crash "Invalid rule"

        _ ->
            Debug.crash "Invalid rule"


parse : String -> Input
parse =
    Parsed << List.foldl parseRules Dict.empty << String.lines


unsafeHead : List a -> a
unsafeHead xs =
    case List.head xs of
        Just x ->
            x

        Nothing ->
            Debug.crash "Empty list"


mapWith : List String -> Int -> Int -> Int -> String
mapWith list width cols i =
    let
        dropped =
            List.drop i list

        a =
            unsafeHead dropped

        b =
            dropped
                |> List.drop cols
                |> unsafeHead

        c =
            if width == 3 then
                dropped
                    |> List.drop (cols + cols)
                    |> unsafeHead
            else
                ""
    in
        a ++ b ++ c


pack : Dict.Dict String (List String) -> List Int -> Int -> Int -> Int -> List (List String) -> Int -> List String -> String
pack rules colRange num width cols result len list =
    if len < num then
        let
            newResult =
                colRange
                    |> List.map (getUnsafe rules << mapWith list width cols)
                    |> (++) result

            newLen =
                len + cols
        in
            list
                |> List.drop (cols * width)
                |> pack rules colRange num width cols newResult newLen
    else
        result
            |> List.Extra.groupsOf cols
            |> List.map List.Extra.transpose
            |> List.concat
            |> List.concat
            |> String.join ""


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Parse ->
            { model
                | parsedInput = parse model.input
            }
                ! [ trigger WithDelay Run ]

        Run ->
            case model.parsedInput of
                NotParsed ->
                    model ! [ trigger WithDelay Parse ]

                Parsed rules ->
                    let
                        len =
                            String.length model.state

                        width =
                            if len % 2 == 0 then
                                2
                            else
                                3

                        num =
                            len // width // width

                        cols =
                            num
                                |> toFloat
                                |> sqrt
                                |> floor

                        colRange =
                            List.range 0 (cols - 1)

                        state =
                            model.state
                                |> String.Extra.break width
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
                        { model
                            | state = state
                            , iterations = iterations
                            , firstPart = firstPart
                            , secondPart = secondPart
                        }
                            ! nextCmd


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
