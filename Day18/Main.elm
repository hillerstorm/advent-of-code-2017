module Day18.Main exposing (main)

import Browser
import Day18.Input exposing (rawInput)
import Dict exposing (Dict)
import Helpers.Helpers exposing (Delay(..), trigger)
import Html exposing (Html, div, text)


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
    { registers : Dict Register Int
    , index : Int
    , lastPlayed : Int
    , result : Maybe Int
    }


type State
    = Running
    | Waiting
    | Terminated


type alias Prgrm =
    { registers : Dict Register Int
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
      , parsedInput = NotParsed
      , first =
            { registers = Dict.empty
            , index = 0
            , lastPlayed = 0
            , result = Nothing
            }
      , second = Nothing
      }
    , trigger NoDelay Parse
    )


mapRegOrVal : String -> RegOrVal
mapRegOrVal str =
    String.toInt str
        |> Maybe.map Value
        |> Maybe.withDefault (Reg str)


parseInstruction : String -> Maybe Instruction
parseInstruction str =
    case String.words str of
        [ "snd", x ] ->
            Just <| Snd <| mapRegOrVal x

        [ "set", x, y ] ->
            Just <| Set x <| mapRegOrVal y

        [ "add", x, y ] ->
            Just <| Add x <| mapRegOrVal y

        [ "mul", x, y ] ->
            Just <| Mul x <| mapRegOrVal y

        [ "mod", x, y ] ->
            Just <| Mod x <| mapRegOrVal y

        [ "rcv", x ] ->
            Just <| Rcv x

        [ "jgz", x, y ] ->
            Just <| Jgz (mapRegOrVal x) (mapRegOrVal y)

        _ ->
            Nothing


parse : String -> List Instruction
parse =
    List.filterMap parseInstruction << String.lines


getValue : Dict Register Int -> RegOrVal -> Int
getValue registers x =
    case x of
        Reg r ->
            Dict.get r registers
                |> Maybe.withDefault 0

        Value v ->
            v


nextFirst : FirstPart -> Instruction -> FirstPart
nextFirst first inst =
    case inst of
        Snd x ->
            let
                lastPlayed =
                    getValue first.registers x
            in
            FirstPart first.registers (first.index + 1) lastPlayed Nothing

        Set x y ->
            let
                value =
                    getValue first.registers y

                registers =
                    Dict.update x (always <| Just value) first.registers
            in
            FirstPart registers (first.index + 1) first.lastPlayed Nothing

        Add x y ->
            let
                value =
                    getValue first.registers y

                registers =
                    Dict.update x (Just << (+) value << Maybe.withDefault 0) first.registers
            in
            FirstPart registers (first.index + 1) first.lastPlayed Nothing

        Mul x y ->
            let
                value =
                    getValue first.registers y

                registers =
                    Dict.update x (Just << (*) value << Maybe.withDefault 0) first.registers
            in
            FirstPart registers (first.index + 1) first.lastPlayed Nothing

        Mod x y ->
            let
                value =
                    getValue first.registers y

                registers =
                    Dict.update x (Just << (\b a -> (\dividend modulus -> modBy modulus dividend) a b) value << Maybe.withDefault 0) first.registers
            in
            FirstPart registers (first.index + 1) first.lastPlayed Nothing

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
            FirstPart first.registers (first.index + 1) first.lastPlayed recovered

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
            FirstPart first.registers index first.lastPlayed Nothing


type alias NextProgramValues =
    { registers : Dict String Int
    , index : Int
    , queue : List Int
    , sent : Maybe Int
    , state : State
    }


nextSecond : Prgrm -> Instruction -> Int -> NextProgramValues
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
            NextProgramValues program.registers index program.queue (Just sent) state

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
            NextProgramValues registers index program.queue Nothing state

        Add x y ->
            let
                value =
                    getValue program.registers y

                registers =
                    Dict.update x (Just << (+) value << Maybe.withDefault 0) program.registers

                index =
                    program.index + 1

                state =
                    if index >= listLength then
                        Terminated

                    else
                        Running
            in
            NextProgramValues registers index program.queue Nothing state

        Mul x y ->
            let
                value =
                    getValue program.registers y

                registers =
                    Dict.update x (Just << (*) value << Maybe.withDefault 0) program.registers

                index =
                    program.index + 1

                state =
                    if index >= listLength then
                        Terminated

                    else
                        Running
            in
            NextProgramValues registers index program.queue Nothing state

        Mod x y ->
            let
                value =
                    getValue program.registers y

                registers =
                    Dict.update x (Just << (\b a -> (\dividend modulus -> modBy modulus dividend) a b) value << Maybe.withDefault 0) program.registers

                index =
                    program.index + 1

                state =
                    if index >= listLength then
                        Terminated

                    else
                        Running
            in
            NextProgramValues registers index program.queue Nothing state

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
                    NextProgramValues registers index xs Nothing state

                [] ->
                    NextProgramValues program.registers program.index program.queue Nothing Waiting

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
            NextProgramValues program.registers index program.queue Nothing state


runSecond : Prgrm -> Prgrm -> Int -> List Instruction -> Int -> Int
runSecond a b sent instructions listLength =
    let
        aValues =
            case a.state of
                Terminated ->
                    NextProgramValues a.registers a.index a.queue Nothing a.state

                _ ->
                    case List.drop a.index instructions |> List.head of
                        Just inst ->
                            nextSecond a inst listLength

                        Nothing ->
                            NextProgramValues a.registers a.index a.queue Nothing Terminated

        bValues =
            case b.state of
                Terminated ->
                    NextProgramValues b.registers b.index b.queue Nothing b.state

                _ ->
                    case List.drop b.index instructions |> List.head of
                        Just inst ->
                            nextSecond b inst listLength

                        Nothing ->
                            NextProgramValues b.registers b.index b.queue Nothing Terminated

        aQ =
            case ( bValues.sent, aValues.state ) of
                ( Just val, Running ) ->
                    aValues.queue ++ [ val ]

                ( Just val, Waiting ) ->
                    aValues.queue ++ [ val ]

                _ ->
                    aValues.queue

        bQ =
            case ( aValues.sent, bValues.state ) of
                ( Just val, Running ) ->
                    bValues.queue ++ [ val ]

                ( Just val, Waiting ) ->
                    bValues.queue ++ [ val ]

                _ ->
                    bValues.queue

        newA =
            { registers = aValues.registers
            , index = aValues.index
            , queue = aQ
            , state = aValues.state
            }

        newB =
            { registers = bValues.registers
            , index = bValues.index
            , queue = bQ
            , state = bValues.state
            }

        newSent =
            case bValues.sent of
                Just _ ->
                    sent + 1

                Nothing ->
                    sent

        result =
            case ( aValues.state, bValues.state ) of
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
            ( { model
                | parsedInput = Parsed ( parsedInput, List.length parsedInput )
              }
            , trigger WithDelay RunFirst
            )

        RunFirst ->
            case model.parsedInput of
                NotParsed ->
                    ( model
                    , trigger WithDelay Parse
                    )

                Parsed ( instructions, _ ) ->
                    case List.drop model.first.index instructions |> List.head of
                        Just inst ->
                            let
                                nextFirstPart =
                                    nextFirst model.first inst

                                nextCmd =
                                    case nextFirstPart.result of
                                        Just _ ->
                                            [ trigger WithDelay RunSecond ]

                                        Nothing ->
                                            [ trigger NoDelay RunFirst ]
                            in
                            ( { model
                                | first = nextFirstPart
                              }
                            , Cmd.batch nextCmd
                            )

                        Nothing ->
                            ( model
                            , trigger WithDelay RunSecond
                            )

        RunSecond ->
            case model.parsedInput of
                NotParsed ->
                    ( model
                    , trigger WithDelay Parse
                    )

                Parsed ( instructions, listLength ) ->
                    let
                        a =
                            Prgrm (Dict.singleton "p" 0) [] 0 Running

                        b =
                            Prgrm (Dict.singleton "p" 1) [] 0 Running

                        result =
                            runSecond a b 0 instructions listLength
                    in
                    ( { model
                        | second = Just result
                      }
                    , Cmd.none
                    )


print : Maybe Int -> String
print =
    Maybe.withDefault "Calculating..." << Maybe.map String.fromInt


view : Model -> Html msg
view model =
    div []
        (case model.parsedInput of
            NotParsed ->
                [ div [] [ text "Parsing..." ] ]

            Parsed _ ->
                [ div [] [ text <| "Part 1: " ++ print model.first.result ]
                , div [] [ text <| "Part 2: " ++ print model.second ]
                ]
        )
