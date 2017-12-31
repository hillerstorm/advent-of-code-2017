module Day25.Main exposing (main)

import Html exposing (..)
import Day25.Input exposing (rawInput)
import Helpers.Helpers exposing (trigger, Delay(..), unsafeToInt, unsafeGet)
import List.Extra
import Dict.LLRB as Dict


type Direction
    = Left
    | Right


type alias ValueDef =
    { value : Int
    , direction : Direction
    , nextState : String
    }


type alias State =
    { zero : ValueDef
    , one : ValueDef
    }


type Input
    = NotParsed
    | Parsed ( Dict.Dict String State, Int, String )


type alias Tape =
    ( List Int, Int, List Int )


type alias Model =
    { input : String
    , parsedInput : Input
    , result : Maybe Int
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
    , result = Nothing
    }
        ! [ trigger WithDelay Parse ]


notEmpty : List String -> List String
notEmpty =
    List.filter (not << String.isEmpty)


isNewState : List String -> Bool
isNewState =
    Maybe.withDefault False << Maybe.map ((==) "In") << List.head


isValueDef : List String -> Bool
isValueDef =
    Maybe.withDefault False << Maybe.map ((==) "If") << List.head


mapValueDef : List (List String) -> ValueDef
mapValueDef str =
    case str of
        [ [ "-", "Write", "the", "value", x ], [ "-", "Move", "one", "slot", "to", "the", y ], [ "-", "Continue", "with", "state", z ] ] ->
            let
                value =
                    x
                        |> String.dropRight 1
                        |> unsafeToInt

                direction =
                    case y of
                        "left." ->
                            Left

                        "right." ->
                            Right

                        _ ->
                            Debug.crash "Invalid direction"

                nextState =
                    String.dropRight 1 z
            in
                { value = value
                , direction = direction
                , nextState = nextState
                }

        _ ->
            Debug.crash "Invalid value def"


toValueDef : List (List (List String)) -> ( ValueDef, ValueDef )
toValueDef list =
    case list of
        [ _ :: x, _ :: y ] ->
            let
                zero =
                    x
                        |> List.Extra.takeWhile (not << isValueDef)
                        |> mapValueDef

                one =
                    y
                        |> List.Extra.takeWhile (not << isValueDef)
                        |> mapValueDef
            in
                ( zero, one )

        _ ->
            Debug.crash "Invalid value def"


mapState : List String -> List (List String) -> ( String, State )
mapState head xs =
    case head of
        [ "In", "state", x ] ->
            let
                name =
                    String.dropRight 1 x

                ( zero, one ) =
                    xs
                        |> List.Extra.takeWhile (not << isNewState)
                        |> List.Extra.tails
                        |> List.filter (Maybe.withDefault False << Maybe.map isValueDef << List.head)
                        |> toValueDef
            in
                ( name
                , { zero = zero
                  , one = one
                  }
                )

        _ ->
            Debug.crash "invalid state"


toState : List (List String) -> ( String, State )
toState list =
    case list of
        [] ->
            Debug.crash "Invalid state"

        x :: xs ->
            mapState x xs


parse : String -> Input
parse input =
    case input |> String.lines |> notEmpty |> List.map (notEmpty << String.split " ") of
        [ "Begin", "in", "state", x ] :: [ "Perform", "a", "diagnostic", "checksum", "after", y, "steps." ] :: xs ->
            let
                currentState =
                    String.dropRight 1 x

                steps =
                    unsafeToInt y

                states =
                    xs
                        |> List.Extra.tails
                        |> List.filter (Maybe.withDefault False << Maybe.map isNewState << List.head)
                        |> List.map toState
                        |> Dict.fromList

                invalidStates =
                    states
                        |> Dict.values
                        |> List.concatMap (\a -> (Dict.get a.zero.nextState states) :: [ Dict.get a.one.nextState states ])
                        |> List.filter ((==) Nothing)
                        |> List.length
            in
                case ( Dict.get currentState states, invalidStates ) of
                    ( Just _, 0 ) ->
                        Parsed ( states, steps, currentState )

                    _ ->
                        Debug.crash "Invalid states"

        _ ->
            Debug.crash "Invalid input"


nextTape : Tape -> ValueDef -> Tape
nextTape ( lft, cur, rgt ) { direction, value } =
    case direction of
        Left ->
            let
                ( x, xs ) =
                    List.Extra.uncons lft
                        |> Maybe.withDefault ( 0, [] )
            in
                ( xs, x, value :: rgt )

        Right ->
            let
                ( x, xs ) =
                    List.Extra.uncons rgt
                        |> Maybe.withDefault ( 0, [] )
            in
                ( value :: lft, x, xs )


runFirst : Dict.Dict String State -> Int -> String -> Tape -> Int
runFirst states steps currentState ( lft, cur, rgt ) =
    if steps == 0 then
        List.sum (lft ++ [ cur ] ++ rgt)
    else
        let
            { zero, one } =
                unsafeGet currentState states

            todo =
                if cur == 0 then
                    zero
                else
                    one
        in
            runFirst states (steps - 1) todo.nextState <| nextTape ( lft, cur, rgt ) todo


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Parse ->
            { model
                | parsedInput = parse model.input
            }
                ! [ trigger WithDelay Run ]

        Run ->
            case model.parsedInput of
                NotParsed ->
                    model ! [ trigger WithDelay Parse ]

                Parsed ( states, steps, startingState ) ->
                    let
                        result =
                            runFirst states steps startingState ( [], 0, [] )
                    in
                        { model
                            | result = Just result
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
                [ div [] [ text <| "Result: " ++ print model.result ]
                ]
        )
