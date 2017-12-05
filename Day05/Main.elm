module Day05.Main exposing (main)

import Html exposing (..)
import List.Extra exposing (getAt, setAt)
import Day05.Input exposing (parsedInput)
import Helpers.Helpers exposing (trigger, Delay(..))


type State
    = Running
    | Done


type alias Model =
    { input : List Int
    , firstList : List Int
    , firstIndex : Int
    , firstSteps : Int
    , firstState : State
    , secondList : List Int
    , secondIndex : Int
    , secondSteps : Int
    , secondState : State
    }


type Msg
    = Jump


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
    , firstList = parsedInput
    , firstIndex = 0
    , firstSteps = 0
    , firstState = Running
    , secondList = parsedInput
    , secondIndex = 0
    , secondSteps = 0
    , secondState = Running
    }
        ! [ trigger NoDelay Jump ]


type Part
    = First
    | Second


getNextValues : List Int -> Int -> Int -> State -> Part -> ( List Int, Int, Int, State )
getNextValues list index steps state part =
    case state of
        Running ->
            case getAt index list of
                Just x ->
                    let
                        offsetFunc =
                            case part of
                                First ->
                                    (+)

                                Second ->
                                    if x >= 3 then
                                        (-)
                                    else
                                        (+)
                    in
                        case setAt index (offsetFunc x 1) list of
                            Just y ->
                                ( y, index + x, steps + 1, Running )

                            Nothing ->
                                ( list, index + x, steps + 1, Running )

                Nothing ->
                    ( list, index, steps, Done )

        Done ->
            ( list, index, steps, state )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Jump ->
            let
                ( nextFirstList, nextFirstIndex, nextFirstSteps, nextFirstState ) =
                    getNextValues model.firstList model.firstIndex model.firstSteps model.firstState First

                ( nextSecondList, nextSecondIndex, nextSecondSteps, nextSecondState ) =
                    getNextValues model.secondList model.secondIndex model.secondSteps model.secondState Second

                nextCmd =
                    case ( nextFirstState, nextSecondState ) of
                        ( Done, Done ) ->
                            []

                        _ ->
                            [ trigger NoDelay Jump ]
            in
                { model
                    | firstList = nextFirstList
                    , firstIndex = nextFirstIndex
                    , firstSteps = nextFirstSteps
                    , firstState = nextFirstState
                    , secondList = nextSecondList
                    , secondIndex = nextSecondIndex
                    , secondSteps = nextSecondSteps
                    , secondState = nextSecondState
                }
                    ! nextCmd


view : Model -> Html msg
view model =
    div []
        [ div [] [ text <| "Part 1: " ++ toString model.firstSteps ]
        , div [] [ text <| "Part 2: " ++ toString model.secondSteps ]
        ]
