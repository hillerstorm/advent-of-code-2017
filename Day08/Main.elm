module Day08.Main exposing (main)

import Html exposing (..)
import Day08.Input exposing (parsedInput, Input, Instruction)
import Helpers.Helpers exposing (trigger, Delay(..), prettyMaybe)
import Dict exposing (..)


type alias Registers =
    Dict String Int


type alias Model =
    { input : Input
    , queue : Input
    , maxValue : Int
    , registers : Registers
    , firstPart : Maybe Int
    , secondPart : Maybe Int
    }


type Msg
    = NextInstruction


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
    , queue = parsedInput
    , maxValue = 0
    , registers = Dict.empty
    , firstPart = Nothing
    , secondPart = Nothing
    }
        ! [ trigger NoDelay NextInstruction ]


applyFunc : Instruction -> Int -> Maybe Int -> Maybe Int
applyFunc { compFunc, compValue, op, value } compRegister currVal =
    let
        currentValue =
            Maybe.withDefault 0 currVal

        newValue =
            if compFunc compRegister compValue then
                op currentValue value
            else
                currentValue
    in
        Just newValue


applyInstruction : Instruction -> Registers -> Registers
applyInstruction instruction registers =
    let
        compRegister =
            Dict.get instruction.compRegister registers
                |> Maybe.withDefault 0

        fn =
            applyFunc instruction compRegister
    in
        Dict.update instruction.register fn registers


type State
    = Done
    | Running


advance : Input -> Registers -> ( Input, Registers, State )
advance queue registers =
    case queue of
        [] ->
            ( queue, registers, Done )

        x :: xs ->
            let
                nextState =
                    case xs of
                        [] ->
                            Done

                        _ ->
                            Running
            in
                ( xs, applyInstruction x registers, nextState )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NextInstruction ->
            let
                ( newQueue, newRegisters, nextState ) =
                    advance model.queue model.registers

                currentMaxValue =
                    newRegisters
                        |> Dict.values
                        |> List.maximum
                        |> Maybe.withDefault 0

                newMaxValue =
                    if currentMaxValue > model.maxValue then
                        currentMaxValue
                    else
                        model.maxValue

                ( firstPart, secondPart, nextCmd ) =
                    case nextState of
                        Done ->
                            ( Just currentMaxValue, Just newMaxValue, [] )

                        Running ->
                            ( Nothing, Nothing, [ trigger (DelayWithMs 1) NextInstruction ] )
            in
                { model
                    | queue = newQueue
                    , maxValue = newMaxValue
                    , registers = newRegisters
                    , firstPart = firstPart
                    , secondPart = secondPart
                }
                    ! nextCmd


view : Model -> Html msg
view model =
    div []
        (case model.queue of
            [] ->
                [ div [] [ text <| "Part 1: " ++ prettyMaybe model.firstPart ]
                , div [] [ text <| "Part 2: " ++ prettyMaybe model.secondPart ]
                ]

            _ ->
                [ div [] [ text <| "Instructions left: " ++ toString (List.length model.queue) ]
                ]
        )
