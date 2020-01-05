module Day03.Main exposing (main)

import Browser
import Helpers.Helpers exposing (Delay(..), trigger)
import Html exposing (Html, div, text)


parsedInput : Int
parsedInput =
    289326


type alias Position =
    ( Int, Int )


origo : Position
origo =
    ( 0, 0 )


origoSum : Int
origoSum =
    1


type alias Meta =
    { pos : Position
    , sum : Int
    , previous : Square
    }


type Square
    = Origo
    | Other Meta
    | WithoutSum Position


type Direction
    = Left
    | Right
    | Up
    | Down


type alias Model =
    { input : Int
    , steps : Int
    , current : Square
    , triggerCount : Int
    , currentStep : Int
    , resetCounter : Int
    , direction : Direction
    , firstPart : Maybe Int
    , secondPart : Maybe Int
    }


type Msg
    = NextStep


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
    ( { input = parsedInput
      , steps = 2
      , current = Origo
      , triggerCount = -1
      , currentStep = 0
      , resetCounter = 0
      , direction = Right
      , firstPart = Nothing
      , secondPart = Nothing
      }
    , trigger NoDelay NextStep
    )


add : Position -> Position -> Position
add x =
    Tuple.mapFirst ((+) <| Tuple.first x) << Tuple.mapSecond ((+) <| Tuple.second x)


getNextSquare : Position -> Direction -> Position
getNextSquare pos dir =
    case dir of
        Left ->
            add pos ( -1, 0 )

        Right ->
            add pos ( 1, 0 )

        Up ->
            add pos ( 0, -1 )

        Down ->
            add pos ( 0, 1 )


distance : Int -> Int -> Int
distance a b =
    abs (a - b)


adjacentTo : Position -> Position -> Bool
adjacentTo ( x1, y1 ) ( x2, y2 ) =
    distance x1 x2 < 2 && distance y1 y2 < 2


getSum : Position -> Square -> Int
getSum pos square =
    case square of
        Origo ->
            if adjacentTo pos origo then
                origoSum

            else
                0

        Other meta ->
            let
                newSum =
                    if adjacentTo pos meta.pos then
                        meta.sum

                    else
                        0
            in
            newSum + getSum pos meta.previous

        WithoutSum _ ->
            0


getNextDir : Int -> Direction -> Direction
getNextDir a dir =
    if a == 0 then
        case dir of
            Left ->
                Down

            Right ->
                Up

            Up ->
                Left

            Down ->
                Right

    else
        dir


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NextStep ->
            let
                pos =
                    case model.current of
                        Origo ->
                            origo

                        Other meta ->
                            meta.pos

                        WithoutSum p ->
                            p

                squarePos =
                    getNextSquare pos model.direction

                squareSum =
                    case model.secondPart of
                        Just _ ->
                            0

                        Nothing ->
                            getSum squarePos model.current

                square =
                    case model.secondPart of
                        Just _ ->
                            WithoutSum squarePos

                        Nothing ->
                            Other
                                { pos = squarePos
                                , sum = squareSum
                                , previous = model.current
                                }

                nextTriggerCount =
                    if model.resetCounter == 0 then
                        model.triggerCount + 1

                    else
                        model.triggerCount

                nextCounter =
                    if model.resetCounter == 0 then
                        nextTriggerCount * 2

                    else
                        model.resetCounter - 1

                nextStep =
                    if model.currentStep == 0 then
                        nextTriggerCount

                    else
                        model.currentStep - 1

                firstPart =
                    case model.firstPart of
                        Just _ ->
                            model.firstPart

                        Nothing ->
                            if model.steps == model.input then
                                Just <| (abs <| Tuple.first squarePos) + (abs <| Tuple.second squarePos)

                            else
                                Nothing

                secondPart =
                    case model.secondPart of
                        Just _ ->
                            model.secondPart

                        Nothing ->
                            if squareSum > model.input then
                                Just squareSum

                            else
                                Nothing

                nextCmd =
                    case firstPart of
                        Just _ ->
                            []

                        Nothing ->
                            [ trigger NoDelay NextStep ]

                nextSteps =
                    case firstPart of
                        Just _ ->
                            model.steps

                        Nothing ->
                            model.steps + 1
            in
            ( { model
                | current = square
                , triggerCount = nextTriggerCount
                , currentStep = nextStep
                , resetCounter = nextCounter
                , direction = getNextDir model.currentStep model.direction
                , steps = nextSteps
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
        [ div [] [ text <| "Part 1: " ++ print model.firstPart ]
        , div [] [ text <| "Part 2: " ++ print model.secondPart ]
        ]
