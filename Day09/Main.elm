module Day09.Main exposing (main)

import Browser
import Day09.Input exposing (rawInput)
import Helpers.Helpers exposing (Delay(..), prettyMaybe, trigger)
import Html exposing (..)


type alias Group =
    { children : List Thing
    }


type Thing
    = Thing Group
    | Garbage String


type Input
    = NotParsed
    | Parsed Group


type alias Model =
    { input : String
    , parsedInput : Input
    , firstPart : Maybe Int
    , secondPart : Maybe Int
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
      , firstPart = Nothing
      , secondPart = Nothing
      }
    , trigger NoDelay Parse
    )


parseGarbage : String -> String -> Maybe ( String, String )
parseGarbage garbage str =
    case String.uncons str of
        Just ( '!', xs ) ->
            case String.uncons xs of
                Just ( _, xxs ) ->
                    parseGarbage garbage xxs

                Nothing ->
                    Nothing

        Just ( '>', xs ) ->
            Just ( garbage, xs )

        Just ( x, xs ) ->
            parseGarbage (garbage ++ String.fromChar x) xs

        Nothing ->
            Nothing


parseGroup : List Thing -> String -> Maybe ( Group, String )
parseGroup children str =
    case String.uncons str of
        Just ( ',', xs ) ->
            parseGroup children xs

        Just ( '}', xs ) ->
            Just ( Group children, xs )

        Just ( '{', xs ) ->
            case parseGroup [] xs of
                Just ( grp, rest ) ->
                    parseGroup (children ++ [ Thing grp ]) rest

                Nothing ->
                    Nothing

        Just ( '<', xs ) ->
            case parseGarbage "" xs of
                Just ( garbage, rest ) ->
                    parseGroup (children ++ [ Garbage garbage ]) rest

                Nothing ->
                    Nothing

        _ ->
            Nothing


parse : String -> Maybe Input
parse str =
    case String.uncons str of
        Just ( '{', xs ) ->
            case parseGroup [] xs of
                Just ( grp, rest ) ->
                    if String.isEmpty rest then
                        Just (Parsed grp)

                    else
                        Nothing

                Nothing ->
                    Nothing

        _ ->
            Nothing


scoreThing : Int -> Thing -> Int
scoreThing score thing =
    case thing of
        Thing { children } ->
            getScore (score + 1) children

        Garbage _ ->
            0


getScore : Int -> List Thing -> Int
getScore score =
    (+) score << List.sum << List.map (scoreThing score)


countGarbage : Thing -> Int
countGarbage thing =
    case thing of
        Thing { children } ->
            garbageLength children

        Garbage garbage ->
            String.length garbage


garbageLength : List Thing -> Int
garbageLength =
    List.sum << List.map countGarbage


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Parse ->
            case parse model.input of
                Just parsedInput ->
                    ( { model
                        | parsedInput = parsedInput
                      }
                    , trigger NoDelay Run
                    )

                Nothing ->
                    ( model
                    , Cmd.none
                    )

        Run ->
            case model.parsedInput of
                NotParsed ->
                    ( model
                    , Cmd.none
                    )

                Parsed grp ->
                    ( { model
                        | firstPart = Just <| getScore 1 grp.children
                        , secondPart = Just <| garbageLength grp.children
                      }
                    , Cmd.none
                    )


view : Model -> Html msg
view model =
    div []
        [ div [] [ text <| "Part 1: " ++ prettyMaybe model.firstPart ]
        , div [] [ text <| "Part 2: " ++ prettyMaybe model.secondPart ]
        ]
