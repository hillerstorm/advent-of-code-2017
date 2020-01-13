module Day17.Main exposing (main)

import Browser
import Helpers.Helpers exposing (Delay(..), trigger)
import Html exposing (Html, div, text)
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
    ( { input = input
      , state = []
      , index = 0
      , firstPart = Nothing
      , secondPart = Nothing
      }
    , trigger NoDelay First
    )


findSecondPart : Int -> Int -> Int -> Int -> Int -> Int
findSecondPart inp index next len i =
    if i < 50000000 then
        let
            nextIndex =
                modBy len (input + index) + 1

            newNext =
                if nextIndex == 1 then
                    i

                else
                    next

            nextLen =
                len + 1
        in
        findSecondPart inp nextIndex newNext nextLen (i + 1)

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
                        modBy len (model.index + model.input)

                ( l, r ) =
                    List.Extra.splitAt (idx + 1) model.state

                newState =
                    l ++ len :: r

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
            ( { model
                | state = newState
                , index = newIndex
                , firstPart = firstPart
              }
            , Cmd.batch nextCmd
            )

        Second ->
            let
                secondPart =
                    findSecondPart model.input 0 0 1 1
            in
            ( { model
                | secondPart = Just secondPart
              }
            , Cmd.none
            )


print : Maybe Int -> String
print =
    Maybe.withDefault "Calculating..." << Maybe.map String.fromInt


view : Model -> Html msg
view model =
    div []
        [ div [] [ text <| "Part 1: " ++ print model.firstPart ]
        , div [] [ text <| "Part 2: " ++ print model.secondPart ]
        ]
