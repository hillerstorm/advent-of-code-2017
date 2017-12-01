module Day01.Main exposing (main)

import Html exposing (..)
import Day01.Input exposing (parsedInput, ParseResult(..))
import Helpers.Helpers exposing (trigger, Delay(..))
import Array exposing (..)


type alias Model =
    { numbers : Array Int
    , totalCount : Int
    , secondStep : Int
    , index : Int
    , firstSum : Int
    , secondSum : Int
    }


type Msg
    = NextNumber


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
    let
        ( numbers, totalCount, secondStep ) =
            case parsedInput of
                Invalid ->
                    ( Array.empty, 0, 0 )

                Valid { numbers, total } ->
                    ( numbers, total, total // 2 )
    in
        { numbers = numbers
        , totalCount = totalCount
        , secondStep = secondStep
        , index = 0
        , firstSum = 0
        , secondSum = 0
        }
            ! [ trigger NoDelay NextNumber ]


compareWith : a -> a -> Maybe a
compareWith x y =
    if x == y then
        Just x
    else
        Nothing


tryGetSum : Int -> (a -> Maybe number) -> Array a -> number
tryGetSum idx comp =
    Maybe.withDefault 0 << Maybe.andThen comp << Array.get idx


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NextNumber ->
            case Array.get model.index model.numbers of
                Just x ->
                    let
                        newIndex =
                            model.index + 1

                        compareWithX =
                            compareWith x

                        firstIndex =
                            newIndex % model.totalCount

                        firstSum =
                            tryGetSum firstIndex compareWithX model.numbers

                        secondIndex =
                            (model.index + model.secondStep) % model.totalCount

                        secondSum =
                            tryGetSum secondIndex compareWithX model.numbers
                    in
                        { model
                            | index = newIndex
                            , firstSum = model.firstSum + firstSum
                            , secondSum = model.secondSum + secondSum
                        }
                            ! [ trigger NoDelay NextNumber ]

                Nothing ->
                    model ! []


view : Model -> Html msg
view model =
    div []
        [ div [] [ text ("Part 1: " ++ (toString model.firstSum)) ]
        , div [] [ text ("Part 2: " ++ (toString model.secondSum)) ]
        ]
