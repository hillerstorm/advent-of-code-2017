module Day23.Main exposing (main)

import Html exposing (..)
import Day23.Input exposing (rawInput)
import Helpers.Helpers exposing (trigger, Delay(..), unsafeToInt)
import Dict.LLRB as Dict


type alias Register =
    String


type RegOrVal
    = Reg Register
    | Value Int


type Instruction
    = Set Register RegOrVal
    | Sub Register RegOrVal
    | Mul Register RegOrVal
    | Jnz RegOrVal Int
    | Jmp Int


type Input
    = NotParsed
    | Parsed (List Instruction)


type alias Part =
    { registers : Dict.Dict Register Int
    , result : Maybe Int
    }


type alias Model =
    { input : String
    , parsedInput : Input
    , first : Part
    , second : Part
    }


type Msg
    = Parse
    | RunFirst
    | RunSecond


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
    , parsedInput = NotParsed
    , first =
        { registers = Dict.empty
        , result = Nothing
        }
    , second =
        { registers = Dict.singleton "a" 1
        , result = Nothing
        }
    }
        ! [ trigger WithDelay Parse ]


mapRegOrVal : String -> RegOrVal
mapRegOrVal str =
    String.toInt str
        |> Result.map Value
        |> Result.withDefault (Reg str)


parseInstruction : String -> Instruction
parseInstruction str =
    case String.words str of
        [ "set", x, y ] ->
            Set x <| mapRegOrVal y

        [ "sub", x, y ] ->
            Sub x <| mapRegOrVal y

        [ "mul", x, y ] ->
            Mul x <| mapRegOrVal y

        [ "jnz", "1", y ] ->
            Jmp <| unsafeToInt y

        [ "jnz", x, y ] ->
            Jnz (mapRegOrVal x) <| unsafeToInt y

        _ ->
            Debug.crash "Invalid instruction"


parse : String -> Input
parse =
    Parsed << List.map parseInstruction << String.lines


getValue : Dict.Dict Register Int -> RegOrVal -> Int
getValue registers x =
    case x of
        Reg r ->
            Dict.get r registers
                |> Maybe.withDefault 0

        Value v ->
            v


next : List Instruction -> Dict.Dict Register Int -> Int -> Int -> ( Dict.Dict Register Int, Int )
next instructions registers index mulExecuteCount =
    case List.drop index instructions |> List.head of
        Just inst ->
            let
                ( newIndex, newRegisters, newExecuteCount ) =
                    case inst of
                        Set x y ->
                            let
                                value =
                                    getValue registers y

                                newRegisters =
                                    Dict.update x (always <| Just value) registers
                            in
                                ( index + 1, newRegisters, mulExecuteCount )

                        Sub x y ->
                            let
                                value =
                                    getValue registers y

                                newRegisters =
                                    Dict.update x (Just << ((flip (-)) value) << Maybe.withDefault 0) registers
                            in
                                ( index + 1, newRegisters, mulExecuteCount )

                        Mul x y ->
                            let
                                value =
                                    getValue registers y

                                newRegisters =
                                    Dict.update x (Just << ((*) value) << Maybe.withDefault 0) registers
                            in
                                ( index + 1, newRegisters, mulExecuteCount + 1 )

                        Jmp y ->
                            ( index + y, registers, mulExecuteCount )

                        Jnz x y ->
                            let
                                value =
                                    getValue registers x

                                newIndex =
                                    if value /= 0 then
                                        index + y
                                    else
                                        index + 1
                            in
                                ( newIndex, registers, mulExecuteCount )
            in
                next instructions newRegisters newIndex newExecuteCount

        Nothing ->
            ( registers, mulExecuteCount )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Parse ->
            { model
                | parsedInput = parse model.input
            }
                ! [ trigger WithDelay RunFirst ]

        RunFirst ->
            case model.parsedInput of
                NotParsed ->
                    model ! [ trigger WithDelay Parse ]

                Parsed instructions ->
                    let
                        ( registers, mulExecuteCount ) =
                            next instructions model.first.registers 0 0
                    in
                        { model
                            | first =
                                { registers = registers
                                , result = Just mulExecuteCount
                                }
                        }
                            ! [ trigger WithDelay RunSecond ]

        RunSecond ->
            case model.parsedInput of
                NotParsed ->
                    model ! [ trigger WithDelay Parse ]

                Parsed instructions ->
                    let
                        ( registers, _ ) =
                            next instructions model.second.registers 0 0
                    in
                        { model
                            | second =
                                { registers = registers
                                , result = Just <| getValue registers <| Reg "h"
                                }
                        }
                            ! []


print : Maybe Int -> String
print =
    Maybe.withDefault "Calculating..." << Maybe.map toString


view : Model -> Html msg
view model =
    div []
        (case model.parsedInput of
            NotParsed ->
                [ div [] [ text "Parsing..." ] ]

            Parsed input ->
                [ div [] [ text <| "Part 1: " ++ print model.first.result ]
                , div [] [ text <| "Part 2: " ++ print model.second.result ]
                ]
        )
