module Day14.Main exposing (main)

import Html exposing (..)
import Day14.Input exposing (rawInput)
import Day10.Main exposing (calcKnotHash)
import Day12.Main exposing (getGroup, countGroups)
import Helpers.Helpers exposing (trigger, Delay(..), prettyMaybe, unsafeToInt, unique, unsafeGet)
import Dict
import ParseInt
import Set


type alias Model =
    { input : String
    , firstPart : Maybe Int
    , secondPart : Maybe Int
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
    { input = rawInput
    , firstPart = Nothing
    , secondPart = Nothing
    }
        ! [ trigger NoDelay Run ]


hexToByte : String -> String
hexToByte string =
    case ParseInt.parseIntHex string of
        Ok val ->
            String.padLeft 4 '0' <| ParseInt.toRadixUnsafe 2 val

        Err _ ->
            Debug.crash "Invalid hex"


getBits : String -> String
getBits key =
    calcKnotHash key
        |> String.split ""
        |> List.map hexToByte
        |> String.join ""


getAdjacent : Int -> List Int -> Set.Set Int
getAdjacent idx cells =
    let
        x =
            idx % 128

        y =
            idx // 128

        insertIfInCells cells ( x, y ) =
            let
                idx =
                    x + 128 * y
            in
                if x >= 0 && x < 128 && y >= 0 && y < 128 && List.member idx cells then
                    Set.insert <| x + 128 * y
                else
                    identity
    in
        Set.empty
            |> insertIfInCells cells ( x - 1, y )
            |> insertIfInCells cells ( x + 1, y )
            |> insertIfInCells cells ( x, y - 1 )
            |> insertIfInCells cells ( x, y + 1 )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Run ->
            let
                cells =
                    List.range 0 127
                        |> List.map (\i -> List.map unsafeToInt <| String.split "" <| getBits <| model.input ++ "-" ++ (toString i))
                        |> List.concat

                usedCells =
                    List.sum cells

                indexed =
                    cells
                        |> List.indexedMap (,)
                        |> List.filter (((==) 1) << Tuple.second)
                        |> List.map Tuple.first

                dict =
                    indexed
                        |> List.map (\i -> ( i, getAdjacent i indexed ))
                        |> Dict.fromList

                firstGroup =
                    case indexed of
                        [] ->
                            Debug.crash "Invalid case"

                        x :: _ ->
                            getGroup (unsafeGet x dict) dict <| Set.singleton x

                totalGroups =
                    countGroups firstGroup dict 1
            in
                { model
                    | firstPart = Just usedCells
                    , secondPart = Just totalGroups
                }
                    ! []


view : Model -> Html msg
view model =
    div []
        [ div [] [ text <| "Part 1: " ++ prettyMaybe model.firstPart ]
        , div [] [ text <| "Part 2: " ++ prettyMaybe model.secondPart ]
        ]
