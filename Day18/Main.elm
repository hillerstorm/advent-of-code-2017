module Day18.Main exposing (main)

import Html exposing (..)
import Day18.Input exposing (rawInput)
import Helpers.Helpers exposing (trigger, Delay(..), prettyMaybe, unsafeToInt)
import Dict.LLRB as Dict


type alias Register =
    String


type RegOrVal
    = Reg Register
    | Value Int


type Instruction
    = Snd Register
    | Set Register RegOrVal
    | Add Register RegOrVal
    | Mul Register RegOrVal
    | Mod Register RegOrVal
    | Rcv RegOrVal
    | Jgz RegOrVal RegOrVal


type Input
    = NotParsed
    | Parsed (List Instruction)


type alias Model =
    { input : String
    , parsedInput : Input
    , registers : Dict.Dict Register Int
    , index : Int
    , lastPlayed : Int
    , firstPart : Maybe Int
    , secondPart : Maybe Int
    }


type Msg
    = Parse
    | Run


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
    , registers = Dict.empty
    , index = 0
    , lastPlayed = 0
    , firstPart = Nothing
    , secondPart = Nothing
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
            Snd x

        [ "set", x, y ] ->
            Set x <| mapRegOrVal y

        [ "add", x, y ] ->
            Add x <| mapRegOrVal y

        [ "mul", x, y ] ->
            Mul x <| mapRegOrVal y

        [ "mod", x, y ] ->
            Mod x <| mapRegOrVal y

        [ "rcv", x ] ->
            Rcv <| mapRegOrVal x

        [ "jgz", x, y ] ->
            Jgz (mapRegOrVal x) (mapRegOrVal y)

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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Parse ->
            { model
                | parsedInput = parse model.input
            }
                ! [ trigger NoDelay Run ]

        Run ->
            case model.parsedInput of
                NotParsed ->
                    model ! [ trigger NoDelay Parse ]

                Parsed instructions ->
                    case List.drop model.index instructions |> List.head of
                        Just inst ->
                            let
                                ( lastPlayed, index, registers, recovered ) =
                                    case inst of
                                        Snd x ->
                                            let
                                                lastPlayed =
                                                    Dict.get x model.registers
                                                        |> Maybe.withDefault 0
                                            in
                                                ( lastPlayed, model.index + 1, model.registers, Nothing )

                                        Set x y ->
                                            let
                                                value =
                                                    getValue model.registers y

                                                registers =
                                                    Dict.update x (always <| Just value) model.registers
                                            in
                                                ( model.lastPlayed, model.index + 1, registers, Nothing )

                                        Add x y ->
                                            let
                                                value =
                                                    getValue model.registers y

                                                registers =
                                                    Dict.update x (Just << ((+) value) << Maybe.withDefault 0) model.registers
                                            in
                                                ( model.lastPlayed, model.index + 1, registers, Nothing )

                                        Mul x y ->
                                            let
                                                value =
                                                    getValue model.registers y

                                                registers =
                                                    Dict.update x (Just << ((*) value) << Maybe.withDefault 0) model.registers
                                            in
                                                ( model.lastPlayed, model.index + 1, registers, Nothing )

                                        Mod x y ->
                                            let
                                                value =
                                                    getValue model.registers y

                                                registers =
                                                    Dict.update x (Just << ((flip (%)) value) << Maybe.withDefault 0) model.registers
                                            in
                                                ( model.lastPlayed, model.index + 1, registers, Nothing )

                                        Rcv x ->
                                            let
                                                value =
                                                    getValue model.registers x

                                                recovered =
                                                    if value > 0 then
                                                        Just model.lastPlayed
                                                    else
                                                        Nothing
                                            in
                                                ( model.lastPlayed, model.index + 1, model.registers, recovered )

                                        Jgz x y ->
                                            let
                                                value =
                                                    getValue model.registers x

                                                offset =
                                                    getValue model.registers y

                                                index =
                                                    if value > 0 then
                                                        model.index + offset
                                                    else
                                                        model.index + 1
                                            in
                                                ( model.lastPlayed, index, model.registers, Nothing )

                                firstPart =
                                    case model.firstPart of
                                        Just _ ->
                                            model.firstPart

                                        Nothing ->
                                            recovered

                                nextCmd =
                                    case firstPart of
                                        Just _ ->
                                            []

                                        Nothing ->
                                            [ trigger NoDelay Run ]
                            in
                                { model
                                    | lastPlayed = lastPlayed
                                    , index = index
                                    , registers = registers
                                    , firstPart = firstPart
                                }
                                    ! nextCmd

                        Nothing ->
                            model ! []


view : Model -> Html msg
view model =
    div []
        (case model.parsedInput of
            NotParsed ->
                [ div [] [ text "Parsing..." ] ]

            Parsed input ->
                [ div [] [ text <| "Part 1: " ++ prettyMaybe model.firstPart ]
                , div [] [ text <| "Part 2: " ++ prettyMaybe model.secondPart ]
                ]
        )
