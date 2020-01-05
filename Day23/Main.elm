module Day23.Main exposing (main)

import Browser
import Day23.Input exposing (rawInput)
import Dict exposing (Dict)
import Helpers.Helpers exposing (Delay(..), trigger)
import Html exposing (Html, div, text)


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
    { registers : Dict Register Int
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
            , result = Nothing
            }
      , second =
            { registers = Dict.singleton "a" 1
            , result = Nothing
            }
      }
    , trigger WithDelay Parse
    )


mapRegOrVal : String -> RegOrVal
mapRegOrVal str =
    String.toInt str
        |> Maybe.map Value
        |> Maybe.withDefault (Reg str)


parseInstruction : String -> Maybe Instruction
parseInstruction str =
    case String.words str of
        [ "set", x, y ] ->
            Just <| Set x <| mapRegOrVal y

        [ "sub", x, y ] ->
            Just <| Sub x <| mapRegOrVal y

        [ "mul", x, y ] ->
            Just <| Mul x <| mapRegOrVal y

        [ "jnz", "1", y ] ->
            Maybe.map Jmp (String.toInt y)

        [ "jnz", x, y ] ->
            Maybe.map (\a -> Jnz (mapRegOrVal x) a) (String.toInt y)

        _ ->
            Nothing


parse : String -> Input
parse =
    Parsed << List.filterMap parseInstruction << String.lines


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

                                regs =
                                    Dict.update x (always <| Just value) registers
                            in
                            ( index + 1, regs, mulExecuteCount )

                        Sub x y ->
                            let
                                value =
                                    getValue registers y

                                regs =
                                    Dict.update x (Just << (\b a -> a - b) value << Maybe.withDefault 0) registers
                            in
                            ( index + 1, regs, mulExecuteCount )

                        Mul x y ->
                            let
                                value =
                                    getValue registers y

                                regs =
                                    Dict.update x (Just << (*) value << Maybe.withDefault 0) registers
                            in
                            ( index + 1, regs, mulExecuteCount + 1 )

                        Jmp y ->
                            ( index + y, registers, mulExecuteCount )

                        Jnz x y ->
                            let
                                value =
                                    getValue registers x

                                idx =
                                    if value /= 0 then
                                        index + y

                                    else
                                        index + 1
                            in
                            ( idx, registers, mulExecuteCount )
            in
            next instructions newRegisters newIndex newExecuteCount

        Nothing ->
            ( registers, mulExecuteCount )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Parse ->
            ( { model
                | parsedInput = parse model.input
              }
            , trigger WithDelay RunFirst
            )

        RunFirst ->
            case model.parsedInput of
                NotParsed ->
                    ( model
                    , trigger WithDelay Parse
                    )

                Parsed instructions ->
                    let
                        ( registers, mulExecuteCount ) =
                            next instructions model.first.registers 0 0
                    in
                    ( { model
                        | first =
                            { registers = registers
                            , result = Just mulExecuteCount
                            }
                      }
                    , trigger WithDelay RunSecond
                    )

        RunSecond ->
            case model.parsedInput of
                NotParsed ->
                    ( model
                    , trigger WithDelay Parse
                    )

                Parsed instructions ->
                    let
                        ( registers, _ ) =
                            next instructions model.second.registers 0 0
                    in
                    ( { model
                        | second =
                            { registers = registers
                            , result = Just <| getValue registers <| Reg "h"
                            }
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
                , div [] [ text <| "Part 2: " ++ print model.second.result ]
                ]
        )
