module Day07.Main exposing (main)

import Html exposing (..)
import Day07.Input exposing (parsedInput, Input, Prgrm)
import Helpers.Helpers exposing (trigger, Delay(..))
import List.Extra


type alias Model =
    { input : Input
    , firstPart : Maybe FullProgram
    , secondPart : Int
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
    , firstPart = Nothing
    , secondPart = 0
    }
        ! [ trigger NoDelay Run ]


type FullProgram
    = FullProgram
        { name : String
        , weight : Int
        , totalWeight : Int
        , above : List FullProgram
        }


findByName : List Prgrm -> String -> Maybe Prgrm
findByName programs name =
    List.Extra.find (\x -> x.name == name) programs


getTotalWeight : List Prgrm -> Prgrm -> Int
getTotalWeight programs program =
    case program.above of
        [] ->
            program.weight

        xs ->
            let
                aboveWeight =
                    List.sum <| List.map (getTotalWeight programs) <| List.filterMap (findByName programs) program.above
            in
                program.weight + aboveWeight


findInvalid : FullProgram -> Maybe Int
findInvalid program =
    case program of
        FullProgram x ->
            case x.above of
                [] ->
                    Nothing

                xs ->
                    Nothing


getWeight : FullProgram -> Int
getWeight program =
    case program of
        FullProgram x ->
            x.totalWeight


buildTree : List Prgrm -> Prgrm -> FullProgram
buildTree programs program =
    let
        above =
            List.map (buildTree programs) <| List.filterMap (findByName programs) program.above
    in
        FullProgram
            { name = program.name
            , weight = program.weight
            , totalWeight = program.weight + (List.sum <| List.map getWeight above)
            , above = above
            }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Run ->
            let
                bottomProgram =
                    model.input
                        |> List.Extra.find (\x -> not <| List.any (\y -> (List.length y.above > 0) && List.member x.name y.above) model.input)
                        |> Maybe.map (buildTree model.input)

                secondPart =
                    case bottomProgram of
                        Just x ->
                            findInvalid x |> Maybe.withDefault 0

                        Nothing ->
                            0
            in
                { model | firstPart = bottomProgram } ! []


view : Model -> Html msg
view model =
    div []
        [ div [] [ text <| "Part 1: " ++ toString model.firstPart ]
        , div [] [ text <| "Part 2: " ++ toString model.secondPart ]
        ]
