module Day08.Main exposing (main)

import Browser
import Day08.Input exposing (rawInput)
import Dict exposing (Dict)
import Helpers.Helpers exposing (Delay(..), prettyMaybe, trigger, unsafeToInt)
import Html exposing (..)


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
      , queue = NotParsed
      , maxValue = 0
      , registers = Dict.empty
      , firstPart = Nothing
      , secondPart = Nothing
      }
    , trigger NoDelay Parse
    )


parseOp : String -> Maybe (Int -> Int -> Int)
parseOp string =
    case string of
        "inc" ->
            Just (+)

        "dec" ->
            Just (-)

        _ ->
            Nothing


parseFunc : String -> Maybe (Int -> Int -> Bool)
parseFunc string =
    case string of
        ">" ->
            Just (>)

        "<" ->
            Just (<)

        ">=" ->
            Just (>=)

        "<=" ->
            Just (<=)

        "==" ->
            Just (==)

        "!=" ->
            Just (/=)

        _ ->
            Nothing


parseInstruction : String -> Maybe Instruction
parseInstruction string =
    case String.words string of
        [ register, op, value, "if", compRegister, compFunc, compValue ] ->
            case ( parseFunc compFunc, parseOp op ) of
                ( Just fn, Just opFn ) ->
                    Just
                        { register = register
                        , op = opFn
                        , value = unsafeToInt value
                        , compRegister = compRegister
                        , compValue = unsafeToInt compValue
                        , compFunc = fn
                        }

                _ ->
                    Nothing

        _ ->
            Nothing


parse : String -> Input
parse =
    Parsed << List.filterMap parseInstruction << String.lines


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
            ( { model
                | queue = parse model.input
              }
            , trigger NoDelay NextInstruction
            )

        NextInstruction ->
            case model.queue of
                NotParsed ->
                    ( model
                    , Cmd.none
                    )

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
                    ( { model
                        | queue = Parsed newQueue
                        , maxValue = newMaxValue
                        , registers = newRegisters
                        , firstPart = firstPart
                        , secondPart = secondPart
                      }
                    , Cmd.batch nextCmd
                    )


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
                        [ div [] [ text <| "Instructions left: " ++ String.fromInt (List.length queue) ]
                        ]
        )
