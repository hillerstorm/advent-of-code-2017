module Day25.Main exposing (main)

import Browser
import Day25.Input exposing (rawInput)
import Dict exposing (Dict)
import Helpers.Helpers exposing (Delay(..), trigger, unsafeGet, unsafeToInt)
import Html exposing (..)
import List.Extra


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
      , result = Nothing
      }
    , trigger WithDelay Parse
    )


notEmpty : List String -> List String
notEmpty =
    List.filter (not << String.isEmpty)


isNewState : List String -> Bool
isNewState =
    Maybe.withDefault False << Maybe.map ((==) "In") << List.head


isValueDef : List String -> Bool
isValueDef =
    Maybe.withDefault False << Maybe.map ((==) "If") << List.head


mapValueDef : List (List String) -> Maybe ValueDef
mapValueDef str =
    case str of
        [ [ "-", "Write", "the", "value", x ], [ "-", "Move", "one", "slot", "to", "the", y ], [ "-", "Continue", "with", "state", z ] ] ->
            let
                value =
                    x
                        |> String.dropRight 1
                        |> String.toInt

                direction =
                    case y of
                        "left." ->
                            Just Left

                        "right." ->
                            Just Right

                        _ ->
                            Nothing

                nextState =
                    String.dropRight 1 z
            in
            Maybe.map2 (\v d -> ValueDef v d nextState) value direction

        _ ->
            Nothing


toValueDef : List (List (List String)) -> Maybe ( ValueDef, ValueDef )
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
            Maybe.map2 (\a b -> ( a, b )) zero one

        _ ->
            Nothing


mapState : List String -> List (List String) -> Maybe ( String, State )
mapState head xs =
    case head of
        [ "In", "state", x ] ->
            let
                name =
                    String.dropRight 1 x

                valueDef =
                    xs
                        |> List.Extra.takeWhile (not << isNewState)
                        |> List.Extra.tails
                        |> List.filter (Maybe.withDefault False << Maybe.map isValueDef << List.head)
                        |> toValueDef
            in
            Maybe.map (\( zero, one ) -> ( name, State zero one )) valueDef

        _ ->
            Nothing


toState : List (List String) -> Maybe ( String, State )
toState list =
    case list of
        [] ->
            Nothing

        x :: xs ->
            mapState x xs


parse : String -> Maybe Input
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
                        |> List.filterMap toState
                        |> Dict.fromList

                invalidStates =
                    states
                        |> Dict.values
                        |> List.concatMap (\a -> Dict.get a.zero.nextState states :: [ Dict.get a.one.nextState states ])
                        |> List.filter ((==) Nothing)
                        |> List.length
            in
            case ( Dict.get currentState states, invalidStates ) of
                ( Just _, 0 ) ->
                    Just <| Parsed ( states, steps, currentState )

                _ ->
                    Nothing

        _ ->
            Nothing


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
            case parse model.input of
                Just parsedInput ->
                    ( { model
                        | parsedInput = parsedInput
                      }
                    , trigger WithDelay Run
                    )

                Nothing ->
                    ( model, Cmd.none )

        Run ->
            case model.parsedInput of
                NotParsed ->
                    ( model
                    , trigger WithDelay Parse
                    )

                Parsed ( states, steps, startingState ) ->
                    let
                        result =
                            runFirst states steps startingState ( [], 0, [] )
                    in
                    ( { model
                        | result = Just result
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

            Parsed input ->
                [ div [] [ text <| "Result: " ++ print model.result ]
                ]
        )
