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


findDivisors : List Int -> Maybe ( Int, Int )
findDivisors list =
    case List.sort list |> List.reverse of
        x :: y :: xs ->
            if x % y == 0 then
                Just ( x, y )
            else
                case findDivisors (x :: xs) of
                    Nothing ->
                        findDivisors <| y :: xs

                    val ->
                        val

        _ ->
            Nothing


diffDiv : List Int -> Int
diffDiv list =
    case findDivisors list of
        Just ( x, y ) ->
            x // y

        Nothing ->
            0


sumLines : (List Int -> Int) -> List (List Int) -> Int
sumLines f =
    List.sum << List.map f


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Run ->
            { model
                | firstSum = sumLines diffMinMax model.input
                , secondSum = sumLines diffDiv model.input
            }
                ! []


view : Model -> Html msg
view model =
    div []
        [ div [] [ text <| "Part 1: " ++ toString model.firstSum ]
        , div [] [ text <| "Part 2: " ++ toString model.secondSum ]
        ]
