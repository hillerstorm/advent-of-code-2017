module Day05.Main exposing (main)

import Browser
import Day05.Input exposing (parsedInput)
import Helpers.Helpers exposing (Delay(..), trigger)
import Html exposing (Html, div, text)
import List.Extra exposing (getAt, setAt)


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
    ( { input = parsedInput
      , firstList = parsedInput
      , firstIndex = 0
      , firstSteps = 0
      , firstState = Running
      , secondList = parsedInput
      , secondIndex = 0
      , secondSteps = 0
      , secondState = Running
      }
    , trigger NoDelay Jump
    )


type Part
    = First
    | Second


type alias Values =
    { list : List Int
    , index : Int
    , steps : Int
    , state : State
    }


getNextValues : List Int -> Int -> Int -> State -> Part -> Values
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

                        newList =
                            setAt index (offsetFunc x 1) list
                    in
                    Values newList (index + x) (steps + 1) Running

                Nothing ->
                    Values list index steps Done

        Done ->
            Values list index steps state


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Jump ->
            let
                nextFirst =
                    getNextValues model.firstList model.firstIndex model.firstSteps model.firstState First

                nextSecond =
                    getNextValues model.secondList model.secondIndex model.secondSteps model.secondState Second

                nextCmd =
                    case ( nextFirst.state, nextSecond.state ) of
                        ( Done, Done ) ->
                            []

                        _ ->
                            [ trigger NoDelay Jump ]
            in
            ( { model
                | firstList = nextFirst.list
                , firstIndex = nextFirst.index
                , firstSteps = nextFirst.steps
                , firstState = nextFirst.state
                , secondList = nextSecond.list
                , secondIndex = nextSecond.index
                , secondSteps = nextSecond.steps
                , secondState = nextSecond.state
              }
            , Cmd.batch nextCmd
            )


view : Model -> Html msg
view model =
    div []
        [ div [] [ text <| "Part 1: " ++ String.fromInt model.firstSteps ]
        , div [] [ text <| "Part 2: " ++ String.fromInt model.secondSteps ]
        ]
