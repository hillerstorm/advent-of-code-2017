module Day17.Main exposing (main)

import Html exposing (..)
import Helpers.Helpers exposing (trigger, Delay(..), prettyMaybe)
import List.Extra


input : Int
input =
    376


type alias Model =
    { input : Int
    , state : List Int
    , index : Int
    , firstPart : Maybe Int
    , secondPart : Maybe Int
    }


type Msg
    = First
    | Second


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
    { input = input
    , state = []
    , index = 0
    , firstPart = Nothing
    , secondPart = Nothing
    }
        ! [ trigger NoDelay First ]


findSecondPart : Int -> Int -> Int -> Int -> Int -> Int -> Int
findSecondPart input index zeroIndex next len i =
    if i < 50000000 then
        let
            nextIndex =
                ((input + index) % len) + 1

            nextZero =
                if nextIndex <= zeroIndex then
                    zeroIndex + 1
                else
                    zeroIndex

            newNext =
                if nextIndex == (nextZero + 1) then
                    i
                else
                    next

            nextLen =
                len + 1
        in
            findSecondPart input nextIndex nextZero newNext nextLen <| i + 1
    else
        next


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        First ->
            let
                len =
                    List.length model.state

                idx =
                    if len == 0 then
                        0
                    else
                        (model.index + model.input) % len

                ( l, r ) =
                    List.Extra.splitAt (idx + 1) model.state

                newState =
                    l ++ [ len ] ++ r

                newIndex =
                    if len == 0 then
                        idx
                    else
                        idx + 1

                firstPart =
                    if len == 2017 then
                        case List.head r of
                            Just val ->
                                Just val

                            Nothing ->
                                List.head l
                    else
                        Nothing

                nextCmd =
                    case firstPart of
                        Just _ ->
                            [ trigger WithDelay Second ]

                        Nothing ->
                            [ trigger NoDelay First ]
            in
                { model
                    | state = newState
                    , index = newIndex
                    , firstPart = firstPart
                }
                    ! nextCmd

        Second ->
            let
                secondPart =
                    findSecondPart model.input 0 0 0 1 1
            in
                { model
                    | secondPart = Just secondPart
                }
                    ! []


print : Maybe Int -> String
print =
    Maybe.withDefault "Calculating..." << Maybe.map toString


view : Model -> Html msg
view model =
    div []
        [ div [] [ text <| "Part 1: " ++ print model.firstPart ]
        , div [] [ text <| "Part 2: " ++ print model.secondPart ]
        ]
