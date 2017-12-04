module Day04.Main exposing (main)

import Html exposing (..)
import Day04.Input exposing (parsedInput)
import Helpers.Helpers exposing (trigger, Delay(..), count, unique)


type alias Model =
    { input : List String
    , firstPartCount : Int
    , secondPartCount : Int
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
    , firstPartCount = 0
    , secondPartCount = 0
    }
        ! [ trigger NoDelay Run ]


isUnique : List comparable -> Bool
isUnique w =
    List.sort w == unique w


isUniqueWithAnagrams : List String -> Bool
isUniqueWithAnagrams =
    isUnique << List.map (String.fromList << List.sort << String.toList)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Run ->
            let
                passwords =
                    model.input
                        |> List.map String.words

                firstPartCount =
                    count isUnique passwords

                secondPartCount =
                    count isUniqueWithAnagrams passwords
            in
                { model
                    | firstPartCount = firstPartCount
                    , secondPartCount = secondPartCount
                }
                    ! []


view : Model -> Html msg
view model =
    div []
        [ div [] [ text <| "Part 1: " ++ toString model.firstPartCount ]
        , div [] [ text <| "Part 2: " ++ toString model.secondPartCount ]
        ]
