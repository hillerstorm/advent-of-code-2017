module Day03.Main exposing (main)

import Html exposing (..)
import Helpers.Helpers exposing (trigger, Delay(..))


parsedInput : Int
parsedInput =
    289326


type alias BoundingBox =
    { iteration : Int
    , step : Int
    , topRight : Int
    , topLeft : Int
    , bottomLeft : Int
    , bottomRight : Int
    }


type alias Model =
    { input : Int
    , steps : Int
    , largerValue : Int
    , boundingBox : BoundingBox
    }


type Msg
    = FirstPart
    | SecondPart


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
    { input = parsedInput
    , steps = 0
    , largerValue = 0
    , boundingBox =
        { iteration = 1
        , step = 2
        , topRight = 3
        , topLeft = 5
        , bottomLeft = 7
        , bottomRight = 9
        }
    }
        ! [ trigger NoDelay FirstPart ]


getBoundingBox : Int -> BoundingBox -> BoundingBox
getBoundingBox input current =
    if current.bottomRight >= input then
        current
    else
        let
            step =
                current.step + 2

            topRight =
                current.bottomRight + step

            topLeft =
                topRight + step

            bottomLeft =
                topLeft + step

            bottomRight =
                bottomLeft + step

            newBoundingBox =
                { iteration = current.iteration + 1
                , step = step
                , topRight = topRight
                , topLeft = topLeft
                , bottomLeft = bottomLeft
                , bottomRight = bottomRight
                }
        in
            getBoundingBox input newBoundingBox


calculateSteps : Int -> BoundingBox -> ( Int, BoundingBox )
calculateSteps input boundingBox =
    let
        finalBoundingBox =
            getBoundingBox input boundingBox

        closestCorner =
            if input <= finalBoundingBox.topRight then
                finalBoundingBox.topRight
            else if input <= finalBoundingBox.topLeft then
                finalBoundingBox.topLeft
            else if input <= finalBoundingBox.bottomLeft then
                finalBoundingBox.bottomLeft
            else
                finalBoundingBox.bottomRight

        midpoint =
            closestCorner - (finalBoundingBox.step // 2)

        offset =
            abs <| midpoint - input
    in
        ( offset + finalBoundingBox.iteration, finalBoundingBox )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FirstPart ->
            if model.input == 1 then
                model ! []
            else
                let
                    ( steps, boundingBox ) =
                        calculateSteps model.input model.boundingBox
                in
                    { model | steps = steps, boundingBox = boundingBox } ! [ trigger NoDelay SecondPart ]

        SecondPart ->
            if model.largerValue > model.input then
                model ! []
            else
                { model | largerValue = model.largerValue + 10000 } ! [ trigger WithDelay SecondPart ]


view : Model -> Html msg
view model =
    div []
        [ div [] [ text <| "Part 1: " ++ toString model.steps ]
        , div [] [ text <| "Part 2: " ++ toString model.largerValue ]
        ]
