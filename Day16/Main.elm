module Day16.Main exposing (main)

import Html exposing (..)
import Day16.Input exposing (rawInput)
import Helpers.Helpers exposing (trigger, Delay(..), prettyMaybe, unsafeToInt, mapTuple)
import List.Extra
import Char


type Move
    = Spin Int
    | Exchange Int Int
    | Partner String String


type alias Model =
    { input : String
    , programs : List String
    , visited : List String
    , firstPart : Maybe String
    , secondPart : Maybe String
    }


type Msg
    = Parse
    | Dance


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }


numPrograms : Int
numPrograms =
    16


initPrograms : List String
initPrograms =
    List.range 0 (numPrograms - 1) |> List.map (String.fromChar << Char.fromCode << (+) 97)


init : ( Model, Cmd Msg )
init =
    { input = rawInput
    , programs = initPrograms
    , visited = [ String.concat initPrograms ]
    , firstPart = Nothing
    , secondPart = Nothing
    }
        ! [ trigger NoDelay Parse ]


parseMove : String -> Move
parseMove str =
    case String.uncons str of
        Just ( 's', num ) ->
            Spin <| unsafeToInt num

        Just ( 'x', ab ) ->
            case String.split "/" ab of
                [ a, b ] ->
                    Exchange (unsafeToInt a) (unsafeToInt b)

                _ ->
                    Debug.crash "Invalid exchange"

        Just ( 'p', ab ) ->
            case String.split "/" ab of
                [ a, b ] ->
                    Partner a b

                _ ->
                    Debug.crash "Invalid partner"

        _ ->
            Debug.crash "Invalid move"


nextMove : String -> ( Maybe Move, String )
nextMove str =
    if String.isEmpty str then
        ( Nothing, str )
    else
        case String.indexes "," str of
            x :: _ ->
                let
                    left =
                        String.left x str

                    right =
                        String.dropLeft (x + 1) str
                in
                    ( Just <| parseMove left, right )

            [] ->
                ( Just <| parseMove str, "" )


exchange : List String -> Int -> Int -> List String
exchange programs a b =
    programs
        |> List.Extra.swapAt a b
        |> Maybe.withDefault programs


dance : String -> List String -> List String
dance moves programs =
    case nextMove moves of
        ( Nothing, _ ) ->
            programs

        ( Just move, xs ) ->
            let
                newPrograms =
                    case move of
                        Spin x ->
                            programs
                                |> List.Extra.splitAt (numPrograms - x)
                                |> mapTuple (flip (++))

                        Exchange a b ->
                            exchange programs a b

                        Partner a b ->
                            Maybe.map2 (exchange programs) (List.Extra.elemIndex a programs) (List.Extra.elemIndex b programs)
                                |> Maybe.withDefault programs
            in
                dance xs newPrograms


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Parse ->
            model ! [ trigger WithDelay Dance ]

        Dance ->
            let
                programs =
                    dance model.input model.programs

                state =
                    String.concat programs

                firstPart =
                    model.firstPart
                        |> Maybe.withDefault state

                secondPart =
                    if List.member state model.visited then
                        let
                            idx =
                                1000000000 % (List.length model.visited)
                        in
                            List.Extra.getAt idx model.visited
                    else
                        model.secondPart

                nextCmd =
                    case secondPart of
                        Just _ ->
                            []

                        Nothing ->
                            [ trigger NoDelay Dance ]
            in
                { model
                    | programs = programs
                    , visited = model.visited ++ [ state ]
                    , firstPart = Just firstPart
                    , secondPart = secondPart
                }
                    ! nextCmd


print : Maybe String -> String
print part =
    case part of
        Just x ->
            x

        Nothing ->
            "Calculating..."


view : Model -> Html msg
view model =
    div []
        [ div [] [ text <| "Part 1: " ++ print model.firstPart ]
        , div [] [ text <| "Part 2: " ++ print model.secondPart ]
        ]
