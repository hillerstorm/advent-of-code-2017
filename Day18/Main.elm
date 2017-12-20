module Day18.Main exposing (main)

import Html exposing (..)
import Day18.Input exposing (rawInput)
import Helpers.Helpers exposing (trigger, Delay(..))
import Dict.LLRB as Dict


type alias Register =
    String


type RegOrVal
    = Reg Register
    | Value Int


type Instruction
    = Snd RegOrVal
    | Set Register RegOrVal
    | Add Register RegOrVal
    | Mul Register RegOrVal
    | Mod Register RegOrVal
    | Rcv Register
    | Jgz RegOrVal RegOrVal


type Input
    = NotParsed
    | Parsed ( List Instruction, Int )


type alias FirstPart =
    { registers : Dict.Dict Register Int
    , index : Int
    , lastPlayed : Int
    , result : Maybe Int
    }


type State
    = Running
    | Waiting
    | Terminated


type alias Prgrm =
    { registers : Dict.Dict Register Int
    , queue : List Int
    , index : Int
    , state : State
    }


type alias Model =
    { input : String
    , parsedInput : Input
    , first : FirstPart
    , second : Maybe Int
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
        , index = 0
        , lastPlayed = 0
        , result = Nothing
        }
    , second = Nothing
    }
        ! [ trigger NoDelay Parse ]


mapRegOrVal : String -> RegOrVal
mapRegOrVal str =
    String.toInt str
        |> Result.map Value
        |> Result.withDefault (Reg str)


parseInstruction : String -> Instruction
parseInstruction str =
    case String.words str of
        [ "snd", x ] ->
            Snd <| mapRegOrVal x

        [ "set", x, y ] ->
            Set x <| mapRegOrVal y

        [ "add", x, y ] ->
            Add x <| mapRegOrVal y

        [ "mul", x, y ] ->
            Mul x <| mapRegOrVal y

        [ "mod", x, y ] ->
            Mod x <| mapRegOrVal y

        [ "rcv", x ] ->
            Rcv x

        [ "jgz", x, y ] ->
            Jgz (mapRegOrVal x) (mapRegOrVal y)

        _ ->
            Debug.crash "Invalid instruction"


parse : String -> List Instruction
parse =
    List.map parseInstruction << String.lines


getValue : Dict.Dict Register Int -> RegOrVal -> Int
getValue registers x =
    case x of
        Reg r ->
            Dict.get r registers
                |> Maybe.withDefault 0

        Value v ->
            v


nextFirst : FirstPart -> Instruction -> ( Int, Int, Dict.Dict String Int, Maybe Int )
nextFirst first inst =
    case inst of
        Snd x ->
            let
                lastPlayed =
                    getValue first.registers x
            in
                ( lastPlayed, first.index + 1, first.registers, Nothing )

        Set x y ->
            let
                value =
                    getValue first.registers y

                registers =
                    Dict.update x (always <| Just value) first.registers
            in
                ( first.lastPlayed, first.index + 1, registers, Nothing )

        Add x y ->
            let
                value =
                    getValue first.registers y

                registers =
                    Dict.update x (Just << ((+) value) << Maybe.withDefault 0) first.registers
            in
                ( first.lastPlayed, first.index + 1, registers, Nothing )

        Mul x y ->
            let
                value =
                    getValue first.registers y

                registers =
                    Dict.update x (Just << ((*) value) << Maybe.withDefault 0) first.registers
            in
                ( first.lastPlayed, first.index + 1, registers, Nothing )

        Mod x y ->
            let
                value =
                    getValue first.registers y

                registers =
                    Dict.update x (Just << ((flip (%)) value) << Maybe.withDefault 0) first.registers
            in
                ( first.lastPlayed, first.index + 1, registers, Nothing )

        Rcv x ->
            let
                value =
                    Dict.get x first.registers
                        |> Maybe.withDefault 0

                recovered =
                    if value > 0 then
                        Just first.lastPlayed
                    else
                        Nothing
            in
                ( first.lastPlayed, first.index + 1, first.registers, recovered )

        Jgz x y ->
            let
                value =
                    getValue first.registers x

                offset =
                    getValue first.registers y

                index =
                    if value > 0 then
                        first.index + offset
                    else
                        first.index + 1
            in
                ( first.lastPlayed, index, first.registers, Nothing )


nextSecond : Prgrm -> Instruction -> Int -> ( Dict.Dict String Int, Int, List Int, Maybe Int, State )
nextSecond program inst listLength =
    case inst of
        Snd x ->
            let
                sent =
                    getValue program.registers x

                index =
                    program.index + 1

                state =
                    if index >= listLength then
                        Terminated
                    else
                        Running
            in
                ( program.registers, index, program.queue, Just sent, state )

        Set x y ->
            let
                value =
                    getValue program.registers y

                registers =
                    Dict.update x (always <| Just value) program.registers

                index =
                    program.index + 1

                state =
                    if index >= listLength then
                        Terminated
                    else
                        Running
            in
                ( registers, index, program.queue, Nothing, state )

        Add x y ->
            let
                value =
                    getValue program.registers y

                registers =
                    Dict.update x (Just << ((+) value) << Maybe.withDefault 0) program.registers

                index =
                    program.index + 1

                state =
                    if index >= listLength then
                        Terminated
                    else
                        Running
            in
                ( registers, index, program.queue, Nothing, state )

        Mul x y ->
            let
                value =
                    getValue program.registers y

                registers =
                    Dict.update x (Just << ((*) value) << Maybe.withDefault 0) program.registers

                index =
                    program.index + 1

                state =
                    if index >= listLength then
                        Terminated
                    else
                        Running
            in
                ( registers, index, program.queue, Nothing, state )

        Mod x y ->
            let
                value =
                    getValue program.registers y

                registers =
                    Dict.update x (Just << ((flip (%)) value) << Maybe.withDefault 0) program.registers

                index =
                    program.index + 1

                state =
                    if index >= listLength then
                        Terminated
                    else
                        Running
            in
                ( registers, index, program.queue, Nothing, state )

        Rcv x ->
            case program.queue of
                value :: xs ->
                    let
                        registers =
                            Dict.update x (always <| Just value) program.registers

                        index =
                            program.index + 1

                        state =
                            if index >= listLength then
                                Terminated
                            else
                                Running
                    in
                        ( registers, index, xs, Nothing, state )

                [] ->
                    ( program.registers, program.index, program.queue, Nothing, Waiting )

        Jgz x y ->
            let
                value =
                    getValue program.registers x

                offset =
                    getValue program.registers y

                index =
                    if value > 0 then
                        program.index + offset
                    else
                        program.index + 1

                state =
                    if index >= listLength then
                        Terminated
                    else
                        Running
            in
                ( program.registers, index, program.queue, Nothing, state )


runSecond : Prgrm -> Prgrm -> Int -> List Instruction -> Int -> Int
runSecond a b sent instructions listLength =
    let
        ( aRegisters, aIndex, aQueue, aSent, aState ) =
            case a.state of
                Terminated ->
                    ( a.registers, a.index, a.queue, Nothing, a.state )

                _ ->
                    case List.drop a.index instructions |> List.head of
                        Just inst ->
                            nextSecond a inst listLength

                        Nothing ->
                            ( a.registers, a.index, a.queue, Nothing, Terminated )

        ( bRegisters, bIndex, bQueue, bSent, bState ) =
            case b.state of
                Terminated ->
                    ( b.registers, b.index, b.queue, Nothing, b.state )

                _ ->
                    case List.drop b.index instructions |> List.head of
                        Just inst ->
                            nextSecond b inst listLength

                        Nothing ->
                            ( b.registers, b.index, b.queue, Nothing, Terminated )

        aQ =
            case ( bSent, aState ) of
                ( Just val, Running ) ->
                    aQueue ++ [ val ]

                ( Just val, Waiting ) ->
                    aQueue ++ [ val ]

                _ ->
                    aQueue

        bQ =
            case ( aSent, bState ) of
                ( Just val, Running ) ->
                    bQueue ++ [ val ]

                ( Just val, Waiting ) ->
                    bQueue ++ [ val ]

                _ ->
                    bQueue

        newA =
            { registers = aRegisters
            , index = aIndex
            , queue = aQ
            , state = aState
            }

        newB =
            { registers = bRegisters
            , index = bIndex
            , queue = bQ
            , state = bState
            }

        newSent =
            case bSent of
                Just _ ->
                    sent + 1

                Nothing ->
                    sent

        result =
            case ( aState, bState ) of
                ( Terminated, _ ) ->
                    Just newSent

                ( Waiting, Waiting ) ->
                    Just newSent

                _ ->
                    Nothing
    in
        case result of
            Just val ->
                val

            Nothing ->
                runSecond newA newB newSent instructions listLength


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Parse ->
            let
                parsedInput =
                    parse model.input
            in
                { model
                    | parsedInput = Parsed ( parsedInput, List.length parsedInput )
                }
                    ! [ trigger WithDelay RunFirst ]

        RunFirst ->
            case model.parsedInput of
                NotParsed ->
                    model ! [ trigger WithDelay Parse ]

                Parsed ( instructions, _ ) ->
                    case List.drop model.first.index instructions |> List.head of
                        Just inst ->
                            let
                                ( lastPlayed, index, registers, result ) =
                                    nextFirst model.first inst

                                nextCmd =
                                    case result of
                                        Just _ ->
                                            [ trigger WithDelay RunSecond ]

                                        Nothing ->
                                            [ trigger NoDelay RunFirst ]
                            in
                                { model
                                    | first =
                                        { registers = registers
                                        , index = index
                                        , lastPlayed = lastPlayed
                                        , result = result
                                        }
                                }
                                    ! nextCmd

                        Nothing ->
                            model ! [ trigger WithDelay RunSecond ]

        RunSecond ->
            case model.parsedInput of
                NotParsed ->
                    model ! [ trigger WithDelay Parse ]

                Parsed ( instructions, listLength ) ->
                    let
                        a =
                            Prgrm (Dict.singleton "p" 0) [] 0 Running

                        b =
                            Prgrm (Dict.singleton "p" 1) [] 0 Running

                        result =
                            runSecond a b 0 instructions listLength
                    in
                        { model
                            | second = Just result
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
                , div [] [ text <| "Part 2: " ++ print model.second ]
                ]
        )
