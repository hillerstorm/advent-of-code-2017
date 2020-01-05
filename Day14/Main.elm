module Day14.Main exposing (main)

import Browser
import Day10.Main exposing (calcKnotHash)
import Day12.Main exposing (countGroups, getGroup)
import Day14.Input exposing (rawInput)
import Dict exposing (fromList)
import Helpers.Helpers exposing (Delay(..), trigger)
import Html exposing (Html, div, text)
import ParseInt
import Set exposing (Set)


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
hexToByte =
    Result.toMaybe << Result.andThen (Ok << String.padLeft 4 '0' << ParseInt.toRadixUnsafe 2) << ParseInt.parseIntHex


getBits : String -> String
getBits =
    String.join ""
        << List.filterMap hexToByte
        << String.split ""
        << calcKnotHash


getAdjacent : Int -> List Int -> Set Int
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
                        |> List.map (List.filterMap String.toInt << String.split "" << getBits << (++) (model.input ++ "-") << String.fromInt)
                        |> List.concat

                indexed =
                    List.indexedMap Tuple.pair cells
                        |> List.filter ((==) 1 << Tuple.second)
                        |> List.map Tuple.first

                dict =
                    indexed
                        |> List.map (\i -> ( i, getAdjacent i indexed ))
                        |> Dict.fromList

                secondPart =
                    case indexed of
                        [] ->
                            countGroups Set.empty dict 1

                        x :: _ ->
                            Dict.get x dict
                                |> Maybe.andThen (\value -> getGroup value dict <| Set.singleton x)
                                |> Maybe.andThen (\grp -> countGroups grp dict 1)
            in
            ( { model
                | firstPart = Just <| List.sum cells
                , secondPart = secondPart
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
