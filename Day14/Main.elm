module Day14.Main exposing (main)

import Browser
import Day10.Main exposing (calcKnotHash)
import Day12.Main exposing (countGroups, getGroup)
import Day14.Input exposing (rawInput)
import Dict exposing (Dict)
import Helpers.Helpers exposing (Delay(..), prettyMaybe, trigger, unique, unsafeGet, unsafeToInt)
import Html exposing (..)
import ParseInt
import Set


type alias Model =
    { input : String
    , firstPart : Maybe Int
    , secondPart : Maybe Int
    }


type Msg
    = Run


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
      , firstPart = Nothing
      , secondPart = Nothing
      }
    , trigger NoDelay Run
    )


hexToByte : String -> Maybe String
hexToByte string =
    case ParseInt.parseIntHex string of
        Ok val ->
            Just <| String.padLeft 4 '0' <| ParseInt.toRadixUnsafe 2 val

        Err _ ->
            Nothing


getBits : String -> String
getBits key =
    calcKnotHash key
        |> String.split ""
        |> List.filterMap hexToByte
        |> String.join ""


getAdjacent : Int -> List Int -> Set.Set Int
getAdjacent idx cells =
    let
        x =
            modBy 128 idx

        y =
            idx // 128

        insertIfInCells c ( xx, yy ) =
            let
                i =
                    xx + 128 * yy
            in
            if xx >= 0 && xx < 128 && yy >= 0 && yy < 128 && List.member i c then
                Set.insert <| xx + 128 * yy

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
                        |> List.map (\i -> List.filterMap String.toInt <| String.split "" <| getBits <| model.input ++ "-" ++ String.fromInt i)
                        |> List.concat

                usedCells =
                    List.sum cells

                indexed =
                    cells
                        |> List.indexedMap (\a b -> ( a, b ))
                        |> List.filter ((==) 1 << Tuple.second)
                        |> List.map Tuple.first

                dict =
                    indexed
                        |> List.map (\i -> ( i, getAdjacent i indexed ))
                        |> Dict.fromList

                firstGroup =
                    case indexed of
                        [] ->
                            Set.empty

                        x :: _ ->
                            getGroup (unsafeGet x dict) dict <| Set.singleton x

                totalGroups =
                    countGroups firstGroup dict 1
            in
            ( { model
                | firstPart = Just usedCells
                , secondPart = Just totalGroups
              }
            , Cmd.none
            )


view : Model -> Html msg
view model =
    div []
        [ div [] [ text <| "Part 1: " ++ prettyMaybe model.firstPart ]
        , div [] [ text <| "Part 2: " ++ prettyMaybe model.secondPart ]
        ]
