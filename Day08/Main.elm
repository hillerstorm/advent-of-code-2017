module Day08.Main exposing (main)

import Html exposing (..)
import Day08.Input exposing (rawInput)
import Helpers.Helpers exposing (trigger, Delay(..), prettyMaybe, unsafeToInt)
import Dict.LLRB as Dict exposing (..)


type alias Instruction =
    { register : String
    , op : Int -> Int -> Int
    , value : Int
    , compRegister : String
    , compValue : Int
    , compFunc : Int -> Int -> Bool
    }


type Input
    = NotParsed
    | Parsed (List Instruction)


type alias Registers =
    Dict String Int


type alias Model =
    { input : String
    , queue : Input
    , maxValue : Int
    , registers : Registers
    , firstPart : Maybe Int
    , secondPart : Maybe Int
    }


type Msg
    = Parse
    | NextInstruction


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
    , queue = NotParsed
    , maxValue = 0
    , registers = Dict.empty
    , firstPart = Nothing
    , secondPart = Nothing
    }
        ! [ trigger NoDelay Parse ]


parseOp : String -> Int -> Int -> Int
parseOp string =
    case string of
        "inc" ->
            (+)

        "dec" ->
            (-)

        _ ->
            Debug.crash "Invalid op"


parseFunc : String -> Int -> Int -> Bool
parseFunc string =
    case string of
        ">" ->
            (>)

        "<" ->
            (<)

        ">=" ->
            (>=)

        "<=" ->
            (<=)

        "==" ->
            (==)

        "!=" ->
            (/=)

        _ ->
            Debug.crash "Invalid func"


parseInstruction : String -> Instruction
parseInstruction string =
    case String.words string of
        [ register, op, value, "if", compRegister, compFunc, compValue ] ->
            { register = register
            , op = parseOp op
            , value = unsafeToInt value
            , compRegister = compRegister
            , compValue = unsafeToInt compValue
            , compFunc = parseFunc compFunc
            }

        _ ->
            Debug.crash "Invalid instruction"


parse : String -> Input
parse =
    Parsed << List.map parseInstruction << String.lines


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


advance : List Instruction -> Registers -> ( List Instruction, Registers, State )
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
        Parse ->
            { model
                | queue = parse model.input
            }
                ! [ trigger NoDelay NextInstruction ]

        NextInstruction ->
            case model.queue of
                NotParsed ->
                    model ! []

                Parsed queue ->
                    let
                        ( newQueue, newRegisters, nextState ) =
                            advance queue model.registers

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
                            | queue = Parsed newQueue
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
            NotParsed ->
                [ div [] [ text "Parsing..." ] ]

            Parsed queue ->
                case queue of
                    [] ->
                        [ div [] [ text <| "Part 1: " ++ prettyMaybe model.firstPart ]
                        , div [] [ text <| "Part 2: " ++ prettyMaybe model.secondPart ]
                        ]

                    _ ->
                        [ div [] [ text <| "Instructions left: " ++ toString (List.length queue) ]
                        ]
        )
