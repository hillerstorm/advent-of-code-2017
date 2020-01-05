module Day12.Main exposing (countGroups, getGroup, main)

import Browser
import Day12.Input exposing (rawInput)
import Dict exposing (Dict)
import Helpers.Helpers exposing (Delay(..), trigger)
import Html exposing (Html, div, text)
import Set exposing (Set)


type Input
    = NotParsed
    | Parsed (Dict Int (Set Int))


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


toInt : String -> Maybe Int
toInt str =
    let
        cleaned =
            if String.endsWith "," str then
                String.dropRight 1 str

            else
                str
    in
    String.toInt cleaned


parseLine : String -> Maybe ( Int, Set.Set Int )
parseLine line =
    case String.split " " line of
        key :: "<->" :: first :: xs ->
            Maybe.map
                (\iKey ->
                    let
                        rest =
                            List.filterMap toInt (first :: xs)
                                |> List.filter ((/=) iKey)
                                |> Set.fromList
                    in
                    ( iKey, rest )
                )
                (toInt key)

        _ ->
            Nothing


parse : String -> Input
parse =
    Parsed << Dict.fromList << List.filterMap parseLine << String.lines


getGroup : Set.Set Int -> Dict.Dict Int (Set.Set Int) -> Set.Set Int -> Maybe (Set.Set Int)
getGroup set dict result =
    case Set.toList <| Set.diff set result of
        [] ->
            Just result

        x :: xs ->
            Dict.get x dict
                |> Maybe.andThen (\value -> getGroup value dict <| Set.insert x result)
                |> Maybe.andThen (getGroup (Set.fromList xs) dict)


countGroups : Set.Set Int -> Dict.Dict Int (Set.Set Int) -> Int -> Maybe Int
countGroups toRemove dict cnt =
    let
        newDict =
            Dict.filter (\k _ -> not <| Set.member k toRemove) dict
    in
    case Dict.keys newDict of
        [] ->
            Just cnt

        x :: _ ->
            Dict.get x newDict
                |> Maybe.andThen (\value -> getGroup value newDict <| Set.singleton x)
                |> Maybe.andThen (\newToRemove -> countGroups newToRemove newDict <| cnt + 1)


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

                Parsed dict ->
                    ( Dict.get 0 dict
                        |> Maybe.andThen (\zero -> getGroup zero dict <| Set.singleton 0)
                        |> Maybe.map
                            (\groupZero ->
                                { model
                                    | firstPart = Just <| Set.size groupZero
                                    , secondPart = countGroups groupZero dict 1
                                }
                            )
                        |> Maybe.withDefault model
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
