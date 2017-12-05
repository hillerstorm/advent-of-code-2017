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


type Direction
    = Left
    | Right
    | Up
    | Down


type alias Model =
    { input : Int
    , steps : Int
    , boundingBox : BoundingBox
    , current : Square
    , triggerCount : Int
    , currentStep : Int
    , resetCounter : Int
    , direction : Direction
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
    , boundingBox =
        { iteration = 1
        , step = 2
        , topRight = 3
        , topLeft = 5
        , bottomLeft = 7
        , bottomRight = 9
        }
    , current = Origo
    , triggerCount = -1
    , currentStep = 0
    , resetCounter = 0
    , direction = Right
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
    (distance x1 x2) < 2 && (distance y1 y2) < 2


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
        FirstPart ->
            if model.input == 1 then
                model ! []
            else
                let
                    ( steps, boundingBox ) =
                        calculateSteps model.input model.boundingBox
                in
                    { model
                        | steps = steps
                        , boundingBox = boundingBox
                    }
                        ! [ trigger NoDelay <| SecondPart ]

        SecondPart ->
            let
                pos =
                    case model.current of
                        Origo ->
                            origo

                        Other meta ->
                            meta.pos

                squarePos =
                    getNextSquare pos model.direction

                squareSum =
                    getSum squarePos model.current

                square =
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

                nextCmd =
                    if squareSum > model.input then
                        []
                    else
                        [ trigger WithDelay SecondPart ]
            in
                { model
                    | current = square
                    , triggerCount = nextTriggerCount
                    , currentStep = nextStep
                    , resetCounter = nextCounter
                    , direction = getNextDir model.currentStep model.direction
                }
                    ! nextCmd


extractSum : Square -> Int
extractSum square =
    case square of
        Origo ->
            origoSum

        Other meta ->
            meta.sum


view : Model -> Html msg
view model =
    div []
        [ div [] [ text <| "Part 1: " ++ toString model.steps ]
        , div [] [ text <| "Part 2: " ++ (toString <| extractSum model.current) ]
        ]
