module Day09.Main exposing (main)

import Browser
import Day09.Input exposing (rawInput)
import Helpers.Helpers exposing (Delay(..), trigger)
import Html exposing (Html, div, text)


type alias Group =
    List Thing


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
parseGarbage garbage =
    Maybe.andThen
        (\( x, xs ) ->
            case x of
                '!' ->
                    Maybe.andThen (parseGarbage garbage << Tuple.second) <| String.uncons xs

                '>' ->
                    Just ( garbage, xs )

                _ ->
                    parseGarbage (garbage ++ String.fromChar x) xs
        )
        << String.uncons


parseGroup : List Thing -> String -> Maybe ( Group, String )
parseGroup children =
    Maybe.andThen
        (\( x, xs ) ->
            case x of
                ',' ->
                    parseGroup children xs

                '}' ->
                    Just ( children, xs )

                '{' ->
                    Maybe.andThen
                        (\( grp, rest ) ->
                            parseGroup (children ++ [ Thing grp ]) rest
                        )
                    <|
                        parseGroup [] xs

                '<' ->
                    Maybe.andThen
                        (\( garbage, rest ) ->
                            parseGroup (children ++ [ Garbage garbage ]) rest
                        )
                    <|
                        parseGarbage "" xs

                _ ->
                    Nothing
        )
        << String.uncons


parse : String -> Maybe Input
parse =
    Maybe.andThen
        (\( x, xs ) ->
            case x of
                '{' ->
                    Maybe.andThen
                        (\( grp, rest ) ->
                            if String.isEmpty rest then
                                Just (Parsed grp)

                            else
                                Nothing
                        )
                    <|
                        parseGroup [] xs

                _ ->
                    Nothing
        )
        << String.uncons


scoreThing : Int -> Thing -> Int
scoreThing score thing =
    case thing of
        Thing children ->
            getScore (score + 1) children

        Garbage _ ->
            0


getScore : Int -> List Thing -> Int
getScore score =
    (+) score << List.sum << List.map (scoreThing score)


countGarbage : Thing -> Int
countGarbage thing =
    case thing of
        Thing children ->
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

                Parsed children ->
                    ( { model
                        | firstPart = Just <| getScore 1 children
                        , secondPart = Just <| garbageLength children
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
