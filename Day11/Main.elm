module Day11.Main exposing (main)

import Browser
import Day11.Input exposing (rawInput)
import Helpers.Helpers exposing (Delay(..), trigger)
import Html exposing (Html, div, text)


type Move
    = N
    | NE
    | SE
    | S
    | SW
    | NW


type alias HexPosition =
    ( Int, Int, Int )


type alias Model =
    { input : String
    , current : HexPosition
    , maxDistance : Int
    , firstPart : Maybe Int
    , secondPart : Maybe Int
    }


type Msg
    = Next
    | CalcResult


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }


origo : HexPosition
origo =
    ( 0, 0, 0 )


init : () -> ( Model, Cmd Msg )
init _ =
    ( { input = rawInput
      , current = origo
      , maxDistance = 0
      , firstPart = Nothing
      , secondPart = Nothing
      }
    , trigger NoDelay Next
    )


parseMove : String -> Maybe Move
parseMove mv =
    case mv of
        "n" ->
            Just N

        "ne" ->
            Just NE

        "se" ->
            Just SE

        "s" ->
            Just S

        "sw" ->
            Just SW

        "nw" ->
            Just NW

        _ ->
            Nothing


nextMove : String -> ( Maybe Move, String )
nextMove str =
    if String.isEmpty str then
        ( Nothing, str )

    else
        case String.split "," str of
            x :: xs ->
                ( parseMove x, String.join "," xs )

            [] ->
                ( Nothing, str )


move : HexPosition -> Move -> HexPosition
move ( x, y, z ) mov =
    case mov of
        N ->
            ( x, y + 1, z - 1 )

        NE ->
            ( x + 1, y, z - 1 )

        SE ->
            ( x + 1, y - 1, z )

        S ->
            ( x, y - 1, z + 1 )

        SW ->
            ( x - 1, y, z + 1 )

        NW ->
            ( x - 1, y + 1, z )


distance : HexPosition -> HexPosition -> Int
distance ( x1, y1, z1 ) ( x2, y2, z2 ) =
    (abs (x1 - x2) + abs (y1 - y2) + abs (z1 - z2)) // 2


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Next ->
            case nextMove model.input of
                ( Nothing, _ ) ->
                    ( model
                    , trigger NoDelay CalcResult
                    )

                ( Just x, newInput ) ->
                    let
                        newCurrent =
                            move model.current x

                        dist =
                            distance origo newCurrent

                        newMax =
                            if dist > model.maxDistance then
                                dist

                            else
                                model.maxDistance
                    in
                    ( { model
                        | input = newInput
                        , current = newCurrent
                        , maxDistance = newMax
                      }
                    , trigger NoDelay Next
                    )

        CalcResult ->
            ( { model
                | firstPart = Just <| distance origo model.current
                , secondPart = Just model.maxDistance
              }
            , Cmd.none
            )


print : Maybe Int -> String
print =
    Maybe.withDefault "Calculating..." << Maybe.map String.fromInt


view : Model -> Html msg
view model =
    div []
        (if String.isEmpty model.input then
            [ div [] [ text <| "Part 1: " ++ print model.firstPart ]
            , div [] [ text <| "Part 2: " ++ print model.secondPart ]
            ]

         else
            [ div [] [ text "Calculating..." ] ]
        )
