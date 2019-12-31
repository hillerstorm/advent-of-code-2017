module Day01.Main exposing (main)

import Array exposing (..)
import Browser
import Day01.Input exposing (ParseResult(..), parsedInput)
import Helpers.Helpers exposing (Delay(..), trigger)
import Html exposing (..)


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


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        ( nums, totalCount, secondStep ) =
            case parsedInput of
                Invalid ->
                    ( Array.empty, 0, 0 )

                Valid { numbers, total } ->
                    ( numbers, total, total // 2 )
    in
    ( { numbers = nums
      , totalCount = totalCount
      , secondStep = secondStep
      , index = 0
      , firstSum = 0
      , secondSum = 0
      }
    , trigger NoDelay NextNumber
    )


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
                            modBy model.totalCount newIndex

                        firstSum =
                            tryGetSum firstIndex compareWithX model.numbers

                        secondIndex =
                            modBy model.totalCount (model.index + model.secondStep)

                        secondSum =
                            tryGetSum secondIndex compareWithX model.numbers
                    in
                    ( { model
                        | index = newIndex
                        , firstSum = model.firstSum + firstSum
                        , secondSum = model.secondSum + secondSum
                      }
                    , trigger NoDelay NextNumber
                    )

                Nothing ->
                    ( model
                    , Cmd.none
                    )


view : Model -> Html Msg
view model =
    div []
        [ div [] [ text ("Part 1: " ++ String.fromInt model.firstSum) ]
        , div [] [ text ("Part 2: " ++ String.fromInt model.secondSum) ]
        ]
