module Day021.Main exposing (main)

import Html exposing (..)
import Day02.Input exposing (parsedInput)
import Helpers.Helpers exposing (trigger, Delay(..))


type alias Model =
    { input : List (List Int)
    , firstSum : Int
    , secondSum : Int
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


init : ( Model, Cmd Msg )
init =
    { input = parsedInput
    , firstSum = 0
    , secondSum = 0
    }
        ! [ trigger NoDelay Run ]


diffMinMax : List Int -> Int
diffMinMax list =
    case ( List.maximum list, List.minimum list ) of
        ( Just x, Just y ) ->
            x - y

        _ ->
            0


findDivisors : List Int -> Maybe Int
findDivisors list =
    case list of
        x :: y :: xs ->
            if x % y == 0 then
                Just <| x // y
            else
                case findDivisors <| x :: xs of
                    Nothing ->
                        findDivisors <| y :: xs

                    val ->
                        val

        _ ->
            Nothing


diffDiv : List Int -> Int
diffDiv =
    Maybe.withDefault 0 << findDivisors << List.sortWith (flip compare)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Run ->
            { model
                | firstSum = List.sum <| List.map diffMinMax model.input
                , secondSum = List.sum <| List.map diffDiv model.input
            }
                ! []


view : Model -> Html msg
view model =
    div []
        [ div [] [ text <| "Part 1: " ++ toString model.firstSum ]
        , div [] [ text <| "Part 2: " ++ toString model.secondSum ]
        ]
