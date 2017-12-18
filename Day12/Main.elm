module Day12.Main exposing (main, getGroup, countGroups)

import Html exposing (..)
import Day12.Input exposing (rawInput)
import Helpers.Helpers exposing (trigger, Delay(..), prettyMaybe, unsafeGet)
import Dict.LLRB as Dict
import Set


type Input
    = NotParsed
    | Parsed (Dict.Dict Int (Set.Set Int))


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


toInt : String -> Int
toInt str =
    let
        cleaned =
            if String.endsWith "," str then
                String.dropRight 1 str
            else
                str
    in
        case String.toInt cleaned of
            Ok val ->
                val

            Err _ ->
                Debug.crash "Invalid number"


parseLine : String -> ( Int, Set.Set Int )
parseLine line =
    case String.split " " line of
        key :: "<->" :: first :: xs ->
            let
                iKey =
                    toInt key

                rest =
                    first
                        :: xs
                        |> List.map toInt
                        |> List.filter ((/=) iKey)
                        |> Set.fromList
            in
                ( iKey, rest )

        _ ->
            Debug.crash "Invalid line"


parse : String -> Input
parse =
    Parsed << Dict.fromList << List.map parseLine << String.lines


getGroup : Set.Set Int -> Dict.Dict Int (Set.Set Int) -> Set.Set Int -> Set.Set Int
getGroup set dict result =
    let
        toCheck =
            Set.diff set result
    in
        case Set.toList toCheck of
            [] ->
                result

            x :: xs ->
                let
                    value =
                        unsafeGet x dict

                    newResult =
                        getGroup value dict <| Set.insert x result
                in
                    getGroup (Set.fromList xs) dict newResult


countGroups : Set.Set Int -> Dict.Dict Int (Set.Set Int) -> Int -> Int
countGroups toRemove dict cnt =
    let
        newDict =
            Dict.filter (\k v -> not <| Set.member k toRemove) dict
    in
        case Dict.keys newDict of
            [] ->
                cnt

            x :: _ ->
                let
                    value =
                        unsafeGet x newDict

                    toRemove =
                        getGroup value newDict <| Set.singleton x
                in
                    countGroups toRemove newDict <| cnt + 1


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Parse ->
            { model
                | parsedInput = parse model.input
            }
                ! [ trigger NoDelay Run ]

        Run ->
            case model.parsedInput of
                NotParsed ->
                    model ! [ trigger NoDelay Parse ]

                Parsed dict ->
                    let
                        zeroValue =
                            unsafeGet 0 dict

                        groupZero =
                            getGroup zeroValue dict <| Set.singleton 0

                        secondPart =
                            countGroups groupZero dict 1
                    in
                        { model
                            | firstPart = Just <| Set.size groupZero
                            , secondPart = Just secondPart
                        }
                            ! []


view : Model -> Html msg
view model =
    div []
        (case model.parsedInput of
            NotParsed ->
                [ div [] [ text "Parsing..." ] ]

            Parsed input ->
                [ div [] [ text <| "Part 1: " ++ prettyMaybe model.firstPart ]
                , div [] [ text <| "Part 2: " ++ prettyMaybe model.secondPart ]
                ]
        )
