module Day10.Main exposing (main)

import Html exposing (..)
import Day10.Input exposing (rawInput)
import Helpers.Helpers exposing (trigger, Delay(..))
import Hex
import Bitwise
import Char


type alias FirstPart =
    { lengths : List Int
    , list : List Int
    , index : Int
    , step : Int
    , result : Maybe Int
    }


type alias SecondPart =
    { lengths : List Int
    , current : List Int
    , rounds : Int
    , list : List Int
    , index : Int
    , step : Int
    , result : Maybe String
    }


type alias Model =
    { input : String
    , first : Maybe FirstPart
    , second : Maybe SecondPart
    }


type Msg
    = Parse
    | NextFirst
    | CalcFirst
    | NextSecond
    | CalcSecond


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }


listLength : Int
listLength =
    256


init : ( Model, Cmd Msg )
init =
    { input = rawInput
    , first = Nothing
    , second = Nothing
    }
        ! [ trigger NoDelay Parse ]


toInt : String -> Maybe Int
toInt str =
    case String.toInt str of
        Ok val ->
            if val > listLength then
                Nothing
            else
                Just val

        Err _ ->
            Debug.crash "Invalid input"


parseFirst : String -> List Int
parseFirst str =
    case str of
        "" ->
            []

        xs ->
            xs
                |> String.split ","
                |> List.filterMap toInt


parseSecond : String -> List Int
parseSecond =
    (flip List.append) [ 17, 31, 73, 47, 23 ] << List.map Char.toCode << String.toList


advance : List Int -> Int -> Int -> List Int -> ( List Int, Int, List Int )
advance list idx step lengths =
    case lengths of
        [] ->
            ( list, idx, lengths )

        x :: xs ->
            let
                afterIdx =
                    List.drop idx list

                afterIdxLength =
                    List.length afterIdx

                sliced =
                    afterIdx ++ List.take idx list

                reversed =
                    sliced
                        |> List.take x
                        |> List.reverse

                reSliced =
                    reversed ++ List.drop x sliced

                newList =
                    (List.drop afterIdxLength reSliced) ++ List.take afterIdxLength reSliced

                newIndex =
                    (idx + x + step) % listLength
            in
                ( newList, newIndex, xs )


calcDense : List Int -> List Int -> List Int
calcDense list result =
    case list of
        [] ->
            result

        xs ->
            (List.foldl Bitwise.xor 0 <| List.take 16 xs) :: calcDense (List.drop 16 xs) []


getKnotHash : List Int -> String
getKnotHash =
    List.foldl (flip (++)) "" << List.map (String.padLeft 2 '0' << Hex.toString)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Parse ->
            let
                firstLengths =
                    parseFirst model.input

                secondLengths =
                    parseSecond model.input

                first =
                    Just
                        { lengths = firstLengths
                        , list = List.range 0 <| listLength - 1
                        , index = 0
                        , step = 0
                        , result = Nothing
                        }

                second =
                    Just
                        { lengths = secondLengths
                        , current = secondLengths
                        , rounds = 64
                        , list = List.range 0 <| listLength - 1
                        , index = 0
                        , step = 0
                        , result = Nothing
                        }
            in
                { model
                    | first = first
                    , second = second
                }
                    ! [ trigger NoDelay NextFirst ]

        NextFirst ->
            case model.first of
                Just first ->
                    let
                        ( newList, newIndex, newLengths ) =
                            advance first.list first.index first.step first.lengths

                        newMsg =
                            case newLengths of
                                [] ->
                                    CalcFirst

                                _ ->
                                    NextFirst

                        newFirst =
                            Just
                                { first
                                    | list = newList
                                    , lengths = newLengths
                                    , index = newIndex
                                    , step = first.step + 1
                                }
                    in
                        { model
                            | first = newFirst
                        }
                            ! [ trigger NoDelay newMsg ]

                Nothing ->
                    model ! [ trigger NoDelay Parse ]

        CalcFirst ->
            case model.first of
                Just first ->
                    let
                        result =
                            case first.list of
                                x :: y :: xs ->
                                    Just <| x * y

                                _ ->
                                    Nothing

                        newFirst =
                            Just
                                { first
                                    | result = result
                                }
                    in
                        { model
                            | first = newFirst
                        }
                            ! [ trigger NoDelay NextSecond ]

                Nothing ->
                    model ! [ trigger NoDelay Parse ]

        NextSecond ->
            case model.second of
                Just second ->
                    if second.rounds > 0 then
                        let
                            ( newList, newIndex, newLengths ) =
                                advance second.list second.index second.step second.current

                            newCurrent =
                                case newLengths of
                                    [] ->
                                        second.lengths

                                    _ ->
                                        newLengths

                            newRounds =
                                case newLengths of
                                    [] ->
                                        second.rounds - 1

                                    _ ->
                                        second.rounds

                            newSecond =
                                Just
                                    { second
                                        | current = newCurrent
                                        , rounds = newRounds
                                        , list = newList
                                        , index = newIndex
                                        , step = second.step + 1
                                    }
                        in
                            { model
                                | second = newSecond
                            }
                                ! [ trigger NoDelay NextSecond ]
                    else
                        model ! [ trigger NoDelay CalcSecond ]

                Nothing ->
                    model ! [ trigger NoDelay Parse ]

        CalcSecond ->
            case model.second of
                Just second ->
                    let
                        denseHash =
                            calcDense second.list []

                        knotHash =
                            getKnotHash denseHash

                        newSecond =
                            { second
                                | result = Just knotHash
                            }
                    in
                        { model
                            | second = Just newSecond
                        }
                            ! []

                Nothing ->
                    model ! [ trigger NoDelay Parse ]


print : Maybe { a | result : Maybe b } -> String
print part =
    case part of
        Just x ->
            case x.result of
                Just val ->
                    toString val

                Nothing ->
                    "Calculating..."

        Nothing ->
            "Parsing..."


view : Model -> Html msg
view model =
    div []
        [ div [] [ text <| "Part 1: " ++ print model.first ]
        , div [] [ text <| "Part 2: " ++ print model.second ]
        ]
