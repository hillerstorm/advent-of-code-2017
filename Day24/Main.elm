module Day24.Main exposing (main)

import Browser
import Day24.Input exposing (rawInput)
import Helpers.Helpers exposing (Delay(..), sortDesc, trigger, unsafeToInt)
import Html exposing (..)
import List.Extra


type alias Component =
    ( Int, Int )


type Input
    = NotParsed
    | Parsed (List Component)


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


mapComponent : List String -> Maybe Component
mapComponent parts =
    case parts of
        [ a, b ] ->
            Maybe.map2 (\x y -> ( x, y )) (String.toInt a) (String.toInt b)

        _ ->
            Nothing


parse : String -> Input
parse =
    Parsed << List.filterMap (mapComponent << String.split "/") << String.lines


has : Int -> Component -> Bool
has num ( a, b ) =
    a == num || b == num


isNot : Int -> Component -> Int
isNot num ( a, b ) =
    if a == num then
        b

    else
        a


type alias Bridge =
    ( Int, Int )


scoreOnly : Bridge -> Bridge -> Order
scoreOnly ( _, aScore ) ( _, bScore ) =
    sortDesc aScore bScore


lengthBeforeScore : Bridge -> Bridge -> Order
lengthBeforeScore ( aLen, aScore ) ( bLen, bScore ) =
    case sortDesc aLen bLen of
        EQ ->
            sortDesc aScore bScore

        x ->
            x


buildRest : Int -> (Bridge -> Bridge -> Order) -> ( Component, List Component ) -> Bridge
buildRest num sortFunc ( component, components ) =
    let
        numToCheck =
            isNot num component
    in
    components
        |> buildBridge numToCheck sortFunc
        |> Tuple.mapFirst ((+) 1)
        |> Tuple.mapSecond ((+) <| num + numToCheck)


buildBridge : Int -> (Bridge -> Bridge -> Order) -> List Component -> Bridge
buildBridge num sortFunc components =
    components
        |> List.Extra.select
        |> List.filter (has num << Tuple.first)
        |> List.map (buildRest num sortFunc)
        |> List.sortWith sortFunc
        |> List.head
        |> Maybe.withDefault ( 0, 0 )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Parse ->
            ( { model
                | parsedInput = parse model.input
              }
            , trigger WithDelay RunFirst
            )

        RunFirst ->
            case model.parsedInput of
                NotParsed ->
                    ( model
                    , trigger WithDelay Parse
                    )

                Parsed components ->
                    let
                        ( _, firstPart ) =
                            buildBridge 0 scoreOnly components
                    in
                    ( { model
                        | firstPart = Just firstPart
                      }
                    , trigger WithDelay RunSecond
                    )

        RunSecond ->
            case model.parsedInput of
                NotParsed ->
                    ( model
                    , trigger WithDelay Parse
                    )

                Parsed components ->
                    let
                        ( _, secondPart ) =
                            buildBridge 0 lengthBeforeScore components
                    in
                    ( { model
                        | secondPart = Just secondPart
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

            Parsed input ->
                [ div [] [ text <| "Part 1: " ++ print model.firstPart ]
                , div [] [ text <| "Part 2: " ++ print model.secondPart ]
                ]
        )
