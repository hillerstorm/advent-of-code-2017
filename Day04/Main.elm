module Day04.Main exposing (main)

import Html exposing (..)
import Day04.Input exposing (parsedInput)
import Helpers.Helpers exposing (trigger, Delay(..))
import Dict exposing (..)


type alias Model =
    { input : List String
    , firstPartCount : Int
    , secondPartCount : Int
    }


type Msg
    = Run


type Part
    = PartOne
    | PartTwo


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


isOnlyOne : List a -> a -> Bool
isOnlyOne xs x =
    (List.length <| List.filter ((==) x) xs) == 1


updateDictValue : List a -> a -> Maybe Int -> Maybe Int
updateDictValue chars key =
    let
        cnt =
            List.length <| List.filter ((==) key) chars
    in
        Just << Maybe.withDefault cnt << Maybe.map ((+) cnt)


getUnique : List comparable -> Dict comparable Int
getUnique chars =
    List.foldl (\a -> Dict.update a (updateDictValue chars a)) Dict.empty chars


getChars : String -> ( Int, Dict Char Int )
getChars word =
    let
        chars =
            String.toList word

        unique =
            getUnique chars
    in
        ( List.length chars, unique )


alwaysFail : a -> c -> b -> number
alwaysFail k a =
    always 1


valuesEqual : a -> b -> b -> number -> number
valuesEqual k a b x =
    if a == b then
        x
    else
        x + 1


compareUnique : Dict Char Int -> Dict Char Int -> Bool
compareUnique first second =
    (Dict.merge alwaysFail valuesEqual alwaysFail first second 0) == 0


isAnagram : Int -> Dict Char Int -> String -> Bool
isAnagram charCount unique word =
    let
        ( c, u ) =
            getChars word
    in
        c == charCount && compareUnique unique u


noAnagram : List String -> String -> Bool
noAnagram words word =
    let
        ( charCount, unique ) =
            getChars word

        anagrams =
            words
                |> List.filter (isAnagram charCount unique)
                |> List.length
    in
        anagrams == 1


isValid : Part -> String -> Bool
isValid part str =
    let
        words =
            String.words str

        func =
            case part of
                PartOne ->
                    isOnlyOne words

                PartTwo ->
                    (\w -> isOnlyOne words w && noAnagram words w)
    in
        List.all func words


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Run ->
            let
                firstPartCount =
                    model.input
                        |> List.filter (isValid PartOne)
                        |> List.length

                secondPartCount =
                    model.input
                        |> List.filter (isValid PartTwo)
                        |> List.length
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
