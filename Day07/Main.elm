module Day07.Main exposing (main)

import Browser
import Day07.Input exposing (Input, Prgrm, parsedInput)
import Helpers.Helpers exposing (Delay(..), prettyMaybe, trigger)
import Html exposing (..)
import List.Extra


type alias Model =
    { input : Input
    , firstPart : Maybe Node
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
    ( { input = parsedInput
      , firstPart = Nothing
      , secondPart = Nothing
      }
    , trigger NoDelay Run
    )


type alias Node =
    { name : String
    , weight : Int
    , totalWeight : Int
    , nodes : Nodes
    }


type Nodes
    = Nodes (List Node)


findInvalid : Nodes -> Maybe Int
findInvalid (Nodes nodes) =
    case nodes of
        [] ->
            Nothing

        { totalWeight } :: _ ->
            case List.partition ((==) totalWeight << .totalWeight) nodes of
                ( [ a ], b :: _ ) ->
                    findInvalid a.nodes
                        |> Maybe.withDefault (a.weight - (a.totalWeight - b.totalWeight))
                        |> Just

                ( a :: _, [ b ] ) ->
                    findInvalid b.nodes
                        |> Maybe.withDefault (b.weight - (b.totalWeight - a.totalWeight))
                        |> Just

                _ ->
                    Nothing


findByName : List Prgrm -> String -> Maybe Prgrm
findByName programs name =
    List.Extra.find ((==) name << .name) programs


buildTree : List Prgrm -> Prgrm -> Node
buildTree programs { name, weight, nodes } =
    let
        mappedNodes =
            nodes
                |> List.filterMap (findByName programs)
                |> List.map (buildTree programs)

        nodesWeight =
            mappedNodes
                |> List.map .totalWeight
                |> List.sum
    in
    { name = name
    , weight = weight
    , totalWeight = weight + nodesWeight
    , nodes = Nodes mappedNodes
    }


isBottomNode : List Prgrm -> Prgrm -> Bool
isBottomNode programs { name } =
    not <| List.any (List.member name << .nodes) programs


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Run ->
            let
                bottomNode =
                    model.input
                        |> List.Extra.find (isBottomNode model.input)
                        |> Maybe.map (buildTree model.input)

                secondPart =
                    bottomNode
                        |> Maybe.andThen (findInvalid << .nodes)
            in
            ( { model
                | firstPart = bottomNode
                , secondPart = secondPart
              }
            , Cmd.none
            )


view : Model -> Html msg
view model =
    div []
        [ div [] [ text <| "Part 1: " ++ (model.firstPart |> Maybe.map .name |> prettyMaybe) ]
        , div [] [ text <| "Part 2: " ++ prettyMaybe model.secondPart ]
        ]
