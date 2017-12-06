module Day06.Main exposing (main)

import Html exposing (..)
import Day06.Input exposing (parsedInput, Input)
import Helpers.Helpers exposing (trigger, Delay(..))
import List.Extra


type alias Model =
    { input : Input
    , firstPart : Int
    , secondPart : Int
    }


type Msg
    = Run


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }


inputLength : Int
inputLength =
    List.length parsedInput


init : ( Model, Cmd Msg )
init =
    { input = parsedInput
    , firstPart = 0
    , secondPart = 0
    }
        ! [ trigger NoDelay Run ]


replaceWith : Int -> Int -> Int -> Int -> Int -> Int -> Int
replaceWith currIdx maxIdx allReplace rest i num =
    let
        minIdx =
            currIdx + 1

        additional =
            if rest /= 0 && i /= currIdx then
                if maxIdx > minIdx then
                    if i >= minIdx && i <= maxIdx then
                        1
                    else
                        0
                else if (i >= 0 && i <= maxIdx) || (i >= minIdx) then
                    1
                else
                    0
            else
                0

        actualNum =
            if i == currIdx then
                0
            else
                num
    in
        actualNum + allReplace + additional


advance : Input -> List Input -> ( Input, List Input )
advance current seen =
    let
        maxNum =
            List.maximum current
                |> Maybe.withDefault 0

        idx =
            List.Extra.findIndex ((==) maxNum) current
                |> Maybe.withDefault 0

        allReplace =
            maxNum // inputLength

        rest =
            maxNum % inputLength

        maxIdx =
            (idx + rest) % inputLength

        newCurrent =
            List.indexedMap (replaceWith idx maxIdx allReplace rest) current
    in
        if List.member newCurrent seen then
            ( newCurrent, seen )
        else
            advance newCurrent <| newCurrent :: seen


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Run ->
            let
                ( newCurrent, seen ) =
                    advance model.input [ model.input ]

                firstPart =
                    List.length seen

                secondPart =
                    seen
                        |> List.Extra.elemIndex newCurrent
                        |> Maybe.withDefault -1
                        |> (+) 1
            in
                { model
                    | firstPart = firstPart
                    , secondPart = secondPart
                }
                    ! []


view : Model -> Html msg
view model =
    div []
        [ div [] [ text <| "Part 1: " ++ toString model.firstPart ]
        , div [] [ text <| "Part 2: " ++ toString model.secondPart ]
        ]
